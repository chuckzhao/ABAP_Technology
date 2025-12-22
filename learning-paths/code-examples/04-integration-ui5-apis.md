# Integration, APIs, and UI5 - Code Examples

## Part 1: RESTful ABAP and APIs

### 1.1 HTTP Handler Class for REST API
```abap
CLASS zcl_rest_travel DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
    
  PRIVATE SECTION.
    METHODS handle_get
      IMPORTING request  TYPE REF TO if_web_http_request
                response TYPE REF TO if_web_http_response.
                
    METHODS handle_post
      IMPORTING request  TYPE REF TO if_web_http_request
                response TYPE REF TO if_web_http_response.
                
    METHODS handle_put
      IMPORTING request  TYPE REF TO if_web_http_request
                response TYPE REF TO if_web_http_response.
                
    METHODS handle_delete
      IMPORTING request  TYPE REF TO if_web_http_request
                response TYPE REF TO if_web_http_response.
ENDCLASS.

CLASS zcl_rest_travel IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA(lv_method) = request->get_method( ).
    
    CASE lv_method.
      WHEN if_web_http_client=>get.
        handle_get( request = request response = response ).
        
      WHEN if_web_http_client=>post.
        handle_post( request = request response = response ).
        
      WHEN if_web_http_client=>put.
        handle_put( request = request response = response ).
        
      WHEN if_web_http_client=>delete.
        handle_delete( request = request response = response ).
        
      WHEN OTHERS.
        response->set_status( 405 ). " Method Not Allowed
    ENDCASE.
  ENDMETHOD.

  METHOD handle_get.
    " Get path parameters
    DATA(lv_path) = request->get_header_field( '~path' ).
    
    " Read travel data
    SELECT * FROM ztravel_xxx
      INTO TABLE @DATA(lt_travels)
      UP TO 100 ROWS.
      
    " Convert to JSON
    DATA(lv_json) = /ui2/cl_json=>serialize(
      data = lt_travels
      compress = abap_false
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      
    " Set response
    response->set_text( lv_json ).
    response->set_status( 200 ).
    response->set_header_field(
      i_name = 'Content-Type'
      i_value = 'application/json' ).
  ENDMETHOD.

  METHOD handle_post.
    " Get request body
    DATA(lv_body) = request->get_text( ).
    
    " Parse JSON
    DATA ls_travel TYPE ztravel_xxx.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data = ls_travel ).
        
    " Create travel using RAP
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        CREATE FIELDS ( agency_id customer_id begin_date end_date )
        WITH VALUE #( ( %cid = 'CID_001'
                        agency_id = ls_travel-agency_id
                        customer_id = ls_travel-customer_id
                        begin_date = ls_travel-begin_date
                        end_date = ls_travel-end_date ) )
      MAPPED DATA(ls_mapped)
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).
      
    IF ls_failed IS INITIAL.
      COMMIT ENTITIES.
      response->set_status( 201 ). " Created
      
      " Return created resource
      DATA(lv_response) = /ui2/cl_json=>serialize(
        data = ls_mapped-travel[ 1 ]
        compress = abap_false ).
      response->set_text( lv_response ).
    ELSE.
      response->set_status( 400 ). " Bad Request
      DATA(lv_error) = /ui2/cl_json=>serialize( data = ls_reported ).
      response->set_text( lv_error ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_put.
    " Similar to POST but for updates
    " Implementation here
  ENDMETHOD.

  METHOD handle_delete.
    " Implementation for DELETE
    " Implementation here
  ENDMETHOD.

ENDCLASS.
```

---

### 1.2 Consuming External REST API
```abap
CLASS zcl_external_api_consumer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_weather_data,
             city        TYPE string,
             temperature TYPE string,
             condition   TYPE string,
           END OF ty_weather_data.
           
    METHODS get_weather_data
      IMPORTING
        iv_city          TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_weather_data
      RAISING
        cx_web_http_client_error.
        
ENDCLASS.

CLASS zcl_external_api_consumer IMPLEMENTATION.

  METHOD get_weather_data.
    " Create HTTP client
    DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
      i_destination = cl_http_destination_provider=>create_by_url(
        i_url = 'https://api.weather.com' ) ).
        
    " Build request
    DATA(lo_request) = lo_http_client->get_http_request( ).
    lo_request->set_uri_path( |/v1/weather?city={ iv_city }| ).
    lo_request->set_header_field(
      i_name = 'Authorization'
      i_value = 'Bearer YOUR_API_KEY' ).
    lo_request->set_header_field(
      i_name = 'Accept'
      i_value = 'application/json' ).
      
    " Execute request
    DATA(lo_response) = lo_http_client->execute(
      i_method = if_web_http_client=>get ).
      
    " Check status
    IF lo_response->get_status( )-code = 200.
      " Parse JSON response
      DATA(lv_json) = lo_response->get_text( ).
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = lv_json
        CHANGING
          data = rs_result ).
    ELSE.
      " Handle error
      RAISE EXCEPTION TYPE cx_web_http_client_error
        EXPORTING
          textid = cx_web_http_client_error=>http_error
          http_status = lo_response->get_status( ).
    ENDIF.
    
    " Close connection
    lo_http_client->close( ).
  ENDMETHOD.

ENDCLASS.
```

---

### 1.3 RFC Function Module Wrapper
```abap
CLASS zcl_rfc_wrapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_customer,
             customer_id TYPE kunnr,
             name        TYPE name1,
             city        TYPE ort01,
             country     TYPE land1,
           END OF ty_customer.
           
    METHODS call_bapi_customer_get
      IMPORTING
        iv_customer_id   TYPE kunnr
      EXPORTING
        es_customer_data TYPE ty_customer
        et_messages      TYPE bapiret2_t
      RAISING
        zcx_bapi_error.
        
ENDCLASS.

CLASS zcl_rfc_wrapper IMPLEMENTATION.

  METHOD call_bapi_customer_get.
    DATA: ls_customer_data TYPE bapikna1.
    
    CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL2'
      EXPORTING
        customerno      = iv_customer_id
      IMPORTING
        customeraddress = ls_customer_data
      TABLES
        return          = et_messages.
        
    " Map BAPI structure to internal type
    es_customer_data = CORRESPONDING #( ls_customer_data ).
    
    " Check for errors
    IF line_exists( et_messages[ type = 'E' ] ).
      RAISE EXCEPTION TYPE zcx_bapi_error
        EXPORTING
          messages = et_messages.
    ENDIF.
    
    " Commit if needed
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.

ENDCLASS.
```

---

## Part 2: SAP UI5 Application

### 2.1 manifest.json
```json
{
  "_version": "1.42.0",
  "sap.app": {
    "id": "com.mycompany.travelapp",
    "type": "application",
    "title": "Travel Management",
    "description": "Manage travel bookings",
    "applicationVersion": {
      "version": "1.0.0"
    },
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/Z_UI_TRAVEL_XXX/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0",
          "localUri": "localService/metadata.xml"
        }
      }
    }
  },
  "sap.ui5": {
    "rootView": {
      "viewName": "com.mycompany.travelapp.view.App",
      "type": "XML",
      "id": "app"
    },
    "dependencies": {
      "minUI5Version": "1.108.0",
      "libs": {
        "sap.ui.core": {},
        "sap.m": {},
        "sap.ui.layout": {}
      }
    },
    "models": {
      "i18n": {
        "type": "sap.ui.model.resource.ResourceModel",
        "settings": {
          "bundleName": "com.mycompany.travelapp.i18n.i18n"
        }
      },
      "": {
        "dataSource": "mainService",
        "preload": true,
        "settings": {
          "defaultBindingMode": "TwoWay",
          "defaultCountMode": "Inline",
          "refreshAfterChange": false
        }
      }
    },
    "routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "viewType": "XML",
        "viewPath": "com.mycompany.travelapp.view",
        "controlId": "app",
        "controlAggregation": "pages",
        "async": true
      },
      "routes": [
        {
          "pattern": "",
          "name": "TravelList",
          "target": "TravelList"
        },
        {
          "pattern": "Travel/{travelId}",
          "name": "TravelDetail",
          "target": "TravelDetail"
        }
      ],
      "targets": {
        "TravelList": {
          "viewName": "TravelList",
          "viewLevel": 1
        },
        "TravelDetail": {
          "viewName": "TravelDetail",
          "viewLevel": 2
        }
      }
    }
  }
}
```

---

### 2.2 List View (TravelList.view.xml)
```xml
<mvc:View
    controllerName="com.mycompany.travelapp.controller.TravelList"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    xmlns:core="sap.ui.core"
    displayBlock="true">
    
    <Page
        id="travelListPage"
        title="{i18n>travelListTitle}">
        
        <headerContent>
            <Button
                id="createButton"
                icon="sap-icon://add"
                text="{i18n>createTravel}"
                press=".onCreateTravel"/>
        </headerContent>
        
        <content>
            <Table
                id="travelTable"
                items="{
                    path: '/Travel',
                    parameters: {
                        $expand: 'to_Customer'
                    }
                }"
                mode="SingleSelectMaster"
                selectionChange=".onSelectionChange">
                
                <columns>
                    <Column width="12em">
                        <Text text="{i18n>travelId}"/>
                    </Column>
                    <Column>
                        <Text text="{i18n>customer}"/>
                    </Column>
                    <Column>
                        <Text text="{i18n>beginDate}"/>
                    </Column>
                    <Column>
                        <Text text="{i18n>endDate}"/>
                    </Column>
                    <Column hAlign="End">
                        <Text text="{i18n>totalPrice}"/>
                    </Column>
                    <Column>
                        <Text text="{i18n>status}"/>
                    </Column>
                </columns>
                
                <items>
                    <ColumnListItem type="Navigation">
                        <cells>
                            <ObjectIdentifier
                                title="{TravelId}"/>
                            <Text
                                text="{to_Customer/CustomerName}"/>
                            <Text
                                text="{
                                    path: 'BeginDate',
                                    type: 'sap.ui.model.type.Date',
                                    formatOptions: { pattern: 'dd.MM.yyyy' }
                                }"/>
                            <Text
                                text="{
                                    path: 'EndDate',
                                    type: 'sap.ui.model.type.Date',
                                    formatOptions: { pattern: 'dd.MM.yyyy' }
                                }"/>
                            <ObjectNumber
                                number="{
                                    path: 'TotalPrice',
                                    type: 'sap.ui.model.type.Currency',
                                    formatOptions: { showMeasure: false }
                                }"
                                unit="{CurrencyCode}"/>
                            <ObjectStatus
                                text="{
                                    path: 'OverallStatus',
                                    formatter: '.formatter.statusText'
                                }"
                                state="{
                                    path: 'OverallStatus',
                                    formatter: '.formatter.statusState'
                                }"/>
                        </cells>
                    </ColumnListItem>
                </items>
            </Table>
        </content>
    </Page>
</mvc:View>
```

---

### 2.3 List Controller (TravelList.controller.js)
```javascript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "../model/formatter"
], function (Controller, Filter, FilterOperator, MessageBox, MessageToast, formatter) {
    "use strict";

    return Controller.extend("com.mycompany.travelapp.controller.TravelList", {

        formatter: formatter,

        onInit: function () {
            this._oTable = this.byId("travelTable");
        },

        onCreateTravel: function () {
            this.getOwnerComponent().getRouter().navTo("TravelDetail", {
                travelId: "create"
            });
        },

        onSelectionChange: function (oEvent) {
            const oItem = oEvent.getParameter("listItem");
            const oContext = oItem.getBindingContext();
            const sTravelId = oContext.getProperty("TravelId");

            this.getOwnerComponent().getRouter().navTo("TravelDetail", {
                travelId: sTravelId
            });
        },

        onSearch: function (oEvent) {
            const sQuery = oEvent.getParameter("query");
            const aFilters = [];

            if (sQuery) {
                aFilters.push(new Filter({
                    filters: [
                        new Filter("TravelId", FilterOperator.Contains, sQuery),
                        new Filter("Description", FilterOperator.Contains, sQuery)
                    ],
                    and: false
                }));
            }

            const oBinding = this._oTable.getBinding("items");
            oBinding.filter(aFilters);
        },

        onRefresh: function () {
            const oBinding = this._oTable.getBinding("items");
            oBinding.refresh();
            MessageToast.show(this.getResourceBundle().getText("refreshed"));
        },

        getResourceBundle: function () {
            return this.getView().getModel("i18n").getResourceBundle();
        }
    });
});
```

---

### 2.4 Formatter (model/formatter.js)
```javascript
sap.ui.define([], function () {
    "use strict";

    return {
        statusText: function (sStatus) {
            const oResourceBundle = this.getView().getModel("i18n").getResourceBundle();
            
            switch (sStatus) {
                case "O":
                    return oResourceBundle.getText("statusOpen");
                case "A":
                    return oResourceBundle.getText("statusAccepted");
                case "X":
                    return oResourceBundle.getText("statusRejected");
                default:
                    return sStatus;
            }
        },

        statusState: function (sStatus) {
            switch (sStatus) {
                case "O":
                    return "Warning";
                case "A":
                    return "Success";
                case "X":
                    return "Error";
                default:
                    return "None";
            }
        },

        dateFormat: function (oDate) {
            if (!oDate) {
                return "";
            }
            const oDateFormat = sap.ui.core.format.DateFormat.getDateInstance({
                pattern: "dd.MM.yyyy"
            });
            return oDateFormat.format(oDate);
        },

        currencyValue: function (sValue, sCurrency) {
            if (!sValue) {
                return "";
            }
            const oCurrencyFormat = sap.ui.core.format.NumberFormat.getCurrencyInstance();
            return oCurrencyFormat.format(sValue, sCurrency);
        }
    };
});
```

---

### 2.5 Detail Controller (TravelDetail.controller.js)
```javascript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "../model/formatter"
], function (Controller, JSONModel, MessageBox, MessageToast, formatter) {
    "use strict";

    return Controller.extend("com.mycompany.travelapp.controller.TravelDetail", {

        formatter: formatter,

        onInit: function () {
            const oViewModel = new JSONModel({
                editMode: false,
                busy: false
            });
            this.getView().setModel(oViewModel, "view");

            this.getOwnerComponent().getRouter().getRoute("TravelDetail")
                .attachPatternMatched(this._onRouteMatched, this);
        },

        _onRouteMatched: function (oEvent) {
            const sTravelId = oEvent.getParameter("arguments").travelId;
            
            if (sTravelId === "create") {
                this._createNewTravel();
            } else {
                const sPath = `/Travel('${sTravelId}')`;
                this.getView().bindElement({
                    path: sPath,
                    parameters: {
                        $expand: "to_Customer"
                    }
                });
            }
        },

        _createNewTravel: function () {
            const oModel = this.getView().getModel();
            const oContext = oModel.createEntry("/Travel", {
                properties: {
                    OverallStatus: "O"
                }
            });
            
            this.getView().bindElement({
                path: oContext.getPath()
            });
            
            this.getView().getModel("view").setProperty("/editMode", true);
        },

        onEdit: function () {
            this.getView().getModel("view").setProperty("/editMode", true);
        },

        onSave: function () {
            const oModel = this.getView().getModel();
            this.getView().getModel("view").setProperty("/busy", true);
            
            oModel.submitChanges({
                success: () => {
                    MessageToast.show(this._getText("savedSuccessfully"));
                    this.getView().getModel("view").setProperty("/editMode", false);
                    this.getView().getModel("view").setProperty("/busy", false);
                },
                error: () => {
                    MessageBox.error(this._getText("saveFailed"));
                    this.getView().getModel("view").setProperty("/busy", false);
                }
            });
        },

        onCancel: function () {
            const oModel = this.getView().getModel();
            oModel.resetChanges();
            this.getView().getModel("view").setProperty("/editMode", false);
            this.onNavBack();
        },

        onAcceptTravel: function () {
            this._callAction("acceptTravel", this._getText("travelAccepted"));
        },

        onRejectTravel: function () {
            this._callAction("rejectTravel", this._getText("travelRejected"));
        },

        _callAction: function (sAction, sSuccessMessage) {
            const oModel = this.getView().getModel();
            const oContext = this.getView().getBindingContext();
            
            oModel.callFunction(`/${sAction}`, {
                method: "POST",
                urlParameters: {
                    TravelId: oContext.getProperty("TravelId")
                },
                success: () => {
                    MessageToast.show(sSuccessMessage);
                    oModel.refresh();
                },
                error: () => {
                    MessageBox.error(this._getText("actionFailed"));
                }
            });
        },

        onNavBack: function () {
            this.getOwnerComponent().getRouter().navTo("TravelList");
        },

        _getText: function (sKey) {
            return this.getView().getModel("i18n").getResourceBundle().getText(sKey);
        }
    });
});
```

---

## Quick Reference

### REST API Best Practices
- Use proper HTTP status codes (200, 201, 400, 404, 500)
- Return JSON for API responses
- Implement proper error handling
- Use authentication/authorization
- Document your API endpoints

### UI5 Best Practices
- Follow MVC pattern strictly
- Use formatters for display logic
- Implement proper error handling
- Use resource bundles for i18n
- Bind data efficiently
- Use view models for UI state

### Common HTTP Status Codes
- **200** OK - Success
- **201** Created - Resource created
- **400** Bad Request - Invalid input
- **401** Unauthorized - Authentication required
- **403** Forbidden - No permission
- **404** Not Found - Resource doesn't exist
- **500** Internal Server Error - Server error

---

*Last updated: December 2025*
