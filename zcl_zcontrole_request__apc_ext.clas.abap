class ZCL_ZCONTROLE_REQUEST__APC_EXT definition
  public
  inheriting from ZCL_ZCONTROLE_REQUEST__APC
  create public .

public section.
protected section.

  methods DEFINE_VOCAB_ANNOTATIONS
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZCONTROLE_REQUEST__APC_EXT IMPLEMENTATION.


  METHOD define_vocab_annotations.
**TRY.
*CALL METHOD SUPER->DEFINE_VOCAB_ANNOTATIONS
*    .
** CATCH /iwbep/cx_mgw_med_exception .
**ENDTRY.

*Data Declaration
    DATA: lo_ann_target TYPE REF TO /iwbep/if_mgw_vocan_ann_target.
    DATA: lo_annotation TYPE REF TO /iwbep/if_mgw_vocan_annotation.
    DATA: lo_collection TYPE REF TO /iwbep/if_mgw_vocan_collection.
    DATA: lo_function  TYPE REF TO /iwbep/if_mgw_vocan_function.
    DATA: lo_fun_param  TYPE REF TO /iwbep/if_mgw_vocan_fun_param.
    DATA: lo_property  TYPE REF TO /iwbep/if_mgw_vocan_property.
    DATA: lo_record    TYPE REF TO /iwbep/if_mgw_vocan_record.
    DATA: lo_simp_value TYPE REF TO /iwbep/if_mgw_vocan_simple_val.
    DATA: lo_url        TYPE REF TO /iwbep/if_mgw_vocan_url.
    DATA: lo_label_elem TYPE REF TO /iwbep/if_mgw_vocan_label_elem.
    DATA: lo_reference  TYPE REF TO /iwbep/if_mgw_vocan_reference.

*Calling the generated mehtod for creating annotations
    CALL METHOD super->define_vocab_annotations( ).

*Creating the references for the vocabularies
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_CAPABILITIES'
                                                                iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'Org.OData.Capabilities.V1' ).
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_CORE'
                                                                  iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'Org.OData.Core.V1' ).
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_COMMON'
                                                                  iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.Common.v1' ).
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_COMMUNICATION'
                                                                  iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.Communication.v1' ).
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_MEASURES'
                                                                  iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'Org.OData.Measures.V1' ).
    lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_UI'
                                                                  iv_vocab_version = '0001').
    lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.UI.v1' ).

*Creating the Annotation Target
    lo_ann_target = vocab_anno_model->create_annotations_target( iv_target = 'ZCONTROLE_REQUEST_SRV.RequestSet' ).

*Creating the LineItem Collection
    lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.LineItem' ).
    lo_collection = lo_annotation->create_collection( ).

*Creating the Records
    lo_record = lo_collection->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
    lo_property = lo_record->create_property( iv_property_name = 'Value').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_path( 'Trkorr' ).
    lo_property = lo_record->create_property( iv_property_name = 'Label').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( 'Request' ).
*
    lo_record = lo_collection->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
    lo_property = lo_record->create_property( iv_property_name = 'Value').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_path( 'As4text' ).
    lo_property = lo_record->create_property( iv_property_name = 'Label').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( 'Descrição' ).
*
    lo_record = lo_collection->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
    lo_property = lo_record->create_property( iv_property_name = 'Value').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_path( 'Consultor' ).
    lo_property = lo_record->create_property( iv_property_name = 'Label').
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( 'Consultor' ).
*

  ENDMETHOD.
ENDCLASS.
