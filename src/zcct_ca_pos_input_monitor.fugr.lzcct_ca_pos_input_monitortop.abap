FUNCTION-POOL ZCCT_CA_POS_INPUT_MONITOR. "MESSAGE-ID ..

type-pools: wpusa, bd11.

*{   INSERT         LNDK905640                                        1
constants       c_objtype_sales_order    like swotbasdat-objtype value 'BUS2032'.
*}   INSERT
include lwpsacon.                      " Sales Audit Constants

include lwpueeco.                      " POS Constants

include <cntn01>.                      " workflow container macros
tables: t000,
        wplst,
        wptst.

* structure to determine status of an external document
types: begin of type_int_status,
       segnum       like edidd-segnum,
       segnum_end   like edidd-segnum,
       verbuchung   like wptst-verbuchung,
       wptst_flag,
       status       like poswpsa-status,
       status_prev  like poswpsa-status,
       status_prev2 like poswpsa-status,
       status_disp  like poswpsa-status,
       planned,
       workitem,
       countr       like edids-countr,
       date         like edids-logdat,
       time         like edids-logtim,
       uname        like sy-uname,
       key          like objectconn-objectid.
types: end of type_int_status.

* table for type_int_status
types: type_t_int_status type type_int_status occurs 1.

*BI644889
types: begin of st_sww_contob.
        include structure sww_contob.
types: end of st_sww_contob.
types: tt_sww_contob type st_sww_contob occurs 0.
*EI644889

* Control parameters for current message type
data g_msgpro type wpusa_msgpro.


* Value of SET/GET parameter WPS
data g_parameter_wps(132) value 'X'.

* Global table for storing document links
data g_t_doclink type table of bdwfretvar with header line.

*-----------------------------------------------------------------------
* POS_SA_SCREEN_STATUS
*-----------------------------------------------------------------------

data: g_t_status_display  like poswpsa_hi occurs 0 with header line,
      g_t_reason_display  like poswpsa_re occurs 0 with header line,
      g_t_changes_display like poswpsa_ch occurs 0 with header line,
      g_t_doc_history     type wpusa_t_doc_history with header line,
      g_t_doc_reason      type wpusa_t_doc_reason  with header line,
      g_t_doc_changes     type wpusa_t_doc_changes with header line,
      begin of g_s09,
        docnum      like edidc-docnum,
        segnum      like edidd-segnum,
        segnum_end  like edidd-segnum,
        key         like objectconn-objectid,
        sndprn      like edidc-sndprn,
      end of g_s09,
      g_t_msgpro    type wpusa_t_msgpro   with header line.

data: begin of t_excl occurs 0,
        fcode(4),
      end of t_excl.

controls: status_ctrl  type tableview using screen '900',
          reason_ctrl  type tableview using screen '910',
          changes_ctrl type tableview using screen '910'.


tables: poswpsa, poswpsa_hi, poswpsa_re, poswpsa_ch,
        dd07t.
