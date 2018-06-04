class ZCL_ZCONTROLE_REQUEST__APC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.

  methods DEFINE_VOCAB_ANNOTATIONS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_ZCONTROLE_REQUEST__APC IMPLEMENTATION.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*

define_vocab_annotations( ).
  endmethod.


  method DEFINE_VOCAB_ANNOTATIONS.
data: lo_ann_target type ref to /iwbep/if_mgw_vocan_ann_target.   " Vocabulary Annotation Target                     "#EC NEEDED
DATA: lo_annotation TYPE REF TO /iwbep/if_mgw_vocan_annotation.   " Vocabulary Annotation                            "#EC NEEDED
DATA: lo_collection TYPE REF TO /iwbep/if_mgw_vocan_collection.   " Vocabulary Annotation Collection                 "#EC NEEDED
DATA: lo_function   TYPE REF TO /iwbep/if_mgw_vocan_function.     " Vocabulary Annotation Function                   "#EC NEEDED
DATA: lo_fun_param  TYPE REF TO /iwbep/if_mgw_vocan_fun_param.    " Vocabulary Annotation Function Parameter         "#EC NEEDED
DATA: lo_property   TYPE REF TO /iwbep/if_mgw_vocan_property.     " Vocabulary Annotation Property                   "#EC NEEDED
DATA: lo_record     TYPE REF TO /iwbep/if_mgw_vocan_record.       " Vocabulary Annotation Record                     "#EC NEEDED
DATA: lo_simp_value TYPE REF TO /iwbep/if_mgw_vocan_simple_val.   " Vocabulary Annotation Simple Value               "#EC NEEDED
DATA: lo_url        TYPE REF TO /iwbep/if_mgw_vocan_url.          " Vocabulary Annotation URL                        "#EC NEEDED
DATA: lo_label_elem TYPE REF TO /iwbep/if_mgw_vocan_label_elem.   " Vocabulary Annotation Labeled Element            "#EC NEEDED
DATA: lo_reference  TYPE REF TO /iwbep/if_mgw_vocan_reference.    " Vocabulary Annotation Reference


  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_CORE'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'Org.OData.Core.V1' ).
  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_UI'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.UI.v1' ).
  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_COMMUNICATION'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.Communication.v1' ).
  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_CAPABILITIES'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'Org.OData.Capabilities.V1' ).
  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_COMMON'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'com.sap.vocabularies.Common.v1' ).
  lo_reference = vocab_anno_model->create_vocabulary_reference( iv_vocab_id = '/IWBEP/VOC_MEASURES'
                                                                iv_vocab_version = '0001').
  lo_reference->create_include( iv_namespace = 'Org.OData.Measures.V1' ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.Badge' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.BadgeType' ).
  lo_property = lo_record->create_property( 'HeadLine' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Title' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'ImageUrl' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'TypeImageUrl' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'MainInfo' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'SecondaryInfo' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.Chart' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.ChartDefinitionType' ).
  lo_property = lo_record->create_property( 'Title' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Description' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'ChartType' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_enum_member_by_name( '' ).
  lo_property = lo_record->create_property( 'Measures' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_collection = lo_property->create_collection( ).
  lo_property = lo_record->create_property( 'Dimensions' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_collection = lo_property->create_collection( ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.DataPoint' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataPointType' ).
  lo_property = lo_record->create_property( 'Title' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Description' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'LongDescription' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'ValueFormat' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.NumberFormat' ).
  lo_property = lo_record->create_property( 'ScaleFactor' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'NumberOfFractionalDigits' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_enum_member_by_name( '' ).
  lo_property = lo_record->create_property( 'ReferencePeriod' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.ReferencePeriod' ).
  lo_property = lo_record->create_property( 'Description' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Start' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'End' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'Criticality' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_enum_member_by_name( '' ).
  lo_property = lo_record->create_property( 'CriticalityCalculation' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.CriticalityCalculationType' ).
  lo_property = lo_record->create_property( 'ImprovementDirection' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_enum_member_by_name( '' ).
  lo_property = lo_record->create_property( 'Trend' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_enum_member_by_name( '' ).
  lo_property = lo_record->create_property( 'TrendCalculation' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.TrendCalculationType' ).
  lo_property = lo_record->create_property( 'IsRelativeDifference' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_boolean( ' ' ).
  lo_property = lo_record->create_property( 'UpDifference' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'StrongUpDifference' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'DownDifference' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'StrongDownDifference' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.FieldGroup' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.FieldGroupType' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Data' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_collection = lo_property->create_collection( ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.GeoLocation' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.GeoLocationType' ).
  lo_property = lo_record->create_property( 'Latitude' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_property = lo_record->create_property( 'Longitude' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_path( '' ).
  lo_ann_target = vocab_anno_model->create_annotations_target( 'Request' ).
  lo_ann_target->set_namespace_qualifier( 'ZCONTROLE_REQUEST_SRV' ).
  lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.UI.v1.HeaderInfo' ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.HeaderInfoType' ).
  lo_property = lo_record->create_property( 'TypeName' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'TypeNamePlural' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'TypeImageUrl' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Title' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'Description' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_record = lo_annotation->create_record( iv_record_type = 'com.sap.vocabularies.UI.v1.DataField' ).
  lo_property = lo_record->create_property( 'Label' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  lo_property = lo_record->create_property( 'ImageUrl' ).
  lo_simp_value = lo_property->create_simple_value( ).
  lo_simp_value->set_string( '' ).
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  CONSTANTS: lc_gen_date_time TYPE timestamp VALUE '20170510133333'.                  "#EC NOTEXT
  rv_last_modified = super->get_last_modified( ).
  IF rv_last_modified LT lc_gen_date_time.
    rv_last_modified = lc_gen_date_time.
  ENDIF.
  endmethod.
ENDCLASS.
