CREATE OR REPLACE PACKAGE CUSTOMER."ALZ_CLM_DMS_UTILS"
AS


/*

DÖKÜMAN YÖNETIMI PROJESI  MUHABERAT GIRIS, IS LISTESI ...

.............HAKAN.......................................

*/

TYPE refcur IS REF CURSOR;

milat_tarihi    VARCHAR2(20):= '01/01/2013';

FUNCTION get_doc_name (p_doc_code IN VARCHAR2)
   RETURN VARCHAR2;

FUNCTION get_max_order (p_communication_no   IN NUMBER,
                        p_archive_no         IN NUMBER,
                        p_ext_reference      IN VARCHAR2,
                        p_doc_code           IN VARCHAR2)
   RETURN NUMBER;

-- sonradan spec -Start
    Function Get_Plate_No_By_Type (P_Claim_Id    In Number,
                                   P_Sf_No       In Number,
                                   P_Is_Magdur   In Number) Return Varchar2;

    Function Is_Doc_Invoice (P_Doc_Code          In Varchar2) Return Number;

    Procedure Change_Doc_Status_By_Ext_Ref (P_Is_Insert   In Number, -- Insert:1, Update:0
                                        P_Ext_Reference   In Varchar2,
                                        P_Loginuser       In Varchar2,
                                        P_Doc_Name        In Varchar2);

    Procedure Find_Manual_Detail (
      P_Year              In     Number,
      P_Branch            In     Varchar2,
      P_Num               In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
   );

   Procedure Find_Vergi_No (
      P_Tax               In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
         );

   Procedure Find_Tck_No (
      P_Tck_No               In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
   );

   Procedure Find_Mondial (
      P_Mondial_No        In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
   );

   Procedure Find_Tramer_Detail (
      P_Tramer_Ihbar_No   In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
   );

   Procedure Find_Sz (
      P_Sozlesme_No       In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform in varchar2 default 'DMS'
   );

   Procedure Find_Policy_Detail (
      P_Old_Pol           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_File_Detail (
      P_Year              In     Number,
      P_Branch            In     Varchar2,
      P_Num               In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform  IN VARCHAR2  default 'DMS'
   );

   Procedure Find_Saglik_Bakanligi (
      P_Invoice_No        In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Plate_Detail (
      P_Plate_No          In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Ins_Person_Detail (
      P_Name              In     Varchar2,
      P_Surname           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Insd_Person_Detail (
      P_Name              In     Varchar2,
      P_Surname           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Mdr_Person_Detail (
      P_Name              In     Varchar2,
      P_Surname           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Ins_Inst_Detail (
      P_Name              In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Insd_Inst_Detail (
      P_Name              In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Ph_Person_Detail (
      P_Name              In     Varchar2,
      P_Surname           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Ph_Inst_Detail (
      P_Name              In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Mdr_Inst_Detail (
      P_Name              In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Pol_Detail (
      P_Contract_Id       In     Number,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_File_Agent_Detail (
      P_Contract_Id       In     Number,
      P_Reference_Code    In     Varchar2,
      P_Agent_Id          In     Number,
      P_Old_Pol           In     Varchar2,
      P_Policy_No         In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_Karsi_Sirket (
      P_K_Dosya           In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

   Procedure Find_File_Tt_Detail (
      P_Abone_No          In     Varchar2,
      P_Opus_Clm_List        Out Opus_Clm_List_Rec_Tab,
      P_Process_Results   In Out Customer.Process_Result_Table,
      p_platform IN VARCHAR2 default 'DMS'
   );

-- sonradan spec -End

--GELEN EVRAK KAYIT EKRANI SORGULAMA

procedure GET_OPUS_CLM_LIST
                (p_criteria_type in varchar2,
                p_year in number,p_branch in varchar2, p_num in varchar2,
                p_name in varchar2, p_surname  in varchar2,
                p_person_type  in varchar2, -- i tüzel ,p özel
                p_invoice_no in varchar2,
                p_rim_no  in varchar2 ,
                p_reference_code  in varchar2 /* acente*/,p_old_pol in varchar2, p_policy_no in varchar2,
                p_plate_no  in varchar2,
                p_k_dosya in varchar2,
                p_tax   in varchar2,
                p_tck   in varchar2,
                p_tramer_ihbar_no in varchar2,
                p_mondial_no   in varchar2,
                p_sozlesme_no  in varchar2,
                p_OPUS_CLM_LIST out  OPUS_CLM_LIST_REC_TAB,
                P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE,
                p_platform  IN VARCHAR2 -- default 'DMS'
                );

procedure GET_OPUS_CLM_LIST
                (p_criteria_type in varchar2,
                p_year in number,p_branch in varchar2, p_num in varchar2,
                p_name in varchar2, p_surname  in varchar2,
                p_person_type  in varchar2, -- i tüzel ,p özel
                p_invoice_no in varchar2,
                p_rim_no  in varchar2 ,
                p_reference_code  in varchar2 /* acente*/,p_old_pol in varchar2, p_policy_no in varchar2,
                p_plate_no  in varchar2,
                p_k_dosya in varchar2,
                p_tax   in varchar2,
                p_tck   in varchar2,
                p_tramer_ihbar_no in varchar2,
                p_mondial_no   in varchar2,
                p_sozlesme_no  in varchar2,
                p_OPUS_CLM_LIST out  OPUS_CLM_LIST_REC_TAB,
                P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE--,
              --  p_platform  IN VARCHAR2  default 'DMS'
                );

--GELEN EVRAK KAYIT EKRANI DÖKÜMAN KAYDET  VE GELEN EVRAK KAYIT SORGULAMA VE GÜNCELLE KAYDET
procedure  INSERT_COM_DOCS_INDEX (P_CLM_COM_DOCS_INDEX_TAB in OUT CLM_COM_DOCS_INDEX_TAB,
                    P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE ) ;


--GELEN EVRAK KAYIT SORGULAMA VE GÖRÜNTÜLEME EKARANI SORGU
procedure   GET_COM_DOC_INDEX_CLM_LIST (
                  p_criteria_type      IN     VARCHAR2,
                  p_year               IN     NUMBER,
                  p_branch             IN     VARCHAR2,
                  p_num                IN     VARCHAR2,
                  p_name               IN     VARCHAR2,
                  p_surname            IN     VARCHAR2,
                --  p_mdr_name           IN     VARCHAR2,
               --   p_mdr_surname        IN     VARCHAR2,
                  p_person_type        IN     VARCHAR2,                 -- i tüzel ,p özel
                  p_tck                IN     VARCHAR2,
                  p_tax                IN     VARCHAR2,
                  p_policy_no          IN     VARCHAR2,
                  p_old_pol           IN     VARCHAR2,
                  p_reference_code    IN     VARCHAR2 ,
                  p_Communication_No   IN        NUMBER,
                  p_Doc_Sender         IN       VARCHAR2,                    --Gönderen  Adi
                  p_plate_no           IN     VARCHAR2,
                  p_mdr_plate_no       IN     VARCHAR2,
                  p_Archive_No         IN        NUMBER,
                  p_Index_Date_start   IN        DATE,
                  p_Index_Date_end     IN        DATE,
                  p_Comm_User_Name     IN        VARCHAR2,
                  p_Department_Code    IN        VARCHAR2,
                  p_Invoice_Date_start IN    DATE,
                  p_Invoice_Date_end   IN     DATE,
                  p_invoice_no          IN    varchar2,
                  p_noBranch           IN     NUMBER,   --  1: dosyasiz, 0:dosyali, -1:ikiside
                  p_explanation        IN     VARCHAR2,
                  p_CLM_LIST out  OPUS_CLM_LIST_REC_TAB,
                  P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE,
                  p_platform  IN VARCHAR2  default 'DMS',
                  p_Deliver_Date_start IN DATE,
                  p_Deliver_Date_end IN DATE,
                  p_document_type    IN VARCHAR2, --evrak tipi HUKUK,HASAR
                  p_court_file_info  IN VARCHAR2, --esas no
                  p_court_enf_no     IN NUMBER,   --mahkeme no
                  p_court_enf_type   IN VARCHAR2,  --mahkeme tipi
                  p_barcode_no       IN VARCHAR2,  --barkod no
                  p_folder_no        IN VARCHAR2  --klasör no
                );

--GELEN EVRAK KAYIT SORGULAMA VE GÖRÜNTÜLEME EKARANI SORGU sonrasi eVRAK gIRISI dÜZENLEM EKRANI
procedure  GET_COM_DOC_INDEX_INFO  (
                      p_EXT_REFERENCE varchar2,
                      p_Communication_No Number,
                      p_Archive_No  Number,
                      p_CLM_COM_DOCS_INDEX_TAB out CLM_COM_DOCS_INDEX_TAB,
                      P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE  );


--GELEN EVRAK KAYIT EKRANI SORGULAMA  P_NEW 0 ISE SON KALDIGI PAKET NO, 1 ISE YENI NO VERILECEK
procedure  GET_ARCHIVE_NO_BY_USER (p_user_name in varchar2, P_NEW IN NUMBER, p_Archive_No out  Number );

--EKRANDAN ÇAGRILMIYOR
procedure  GET_COMMUNICATION_NO (p_Communication_No out Number);

--EKSPER EKRANI BARKOD OKUNDUGUNDA BARKODA AIT EVRAK KAYDI
procedure  GET_COM_DOCS_BY_BARKOD (p_barkod in varchar2, p_CLM_COM_DOCS_INDEX_TAB out CLM_COM_DOCS_INDEX_TAB,
                                    P_PROCESS_RESULTS     in out   CUSTOMER.PROCESS_RESULT_TABLE );

--  P_SENDERS IÇINE *_* ILE DOSYA ILGILELERINI AYIRARAK GÖNDER
 PROCEDURE GET_SENDER_LIST (p_EXT_REFERENCE   IN     VARCHAR2,
                              P_SENDERS            OUT VARCHAR2,
                              p_is_invoice in  number,
                              p_spare_part_suppliers  OUT refcur
                              );

procedure  GET_DEPARTMENTS_LIST (P_DEPATMENT_LIST OUT refcur );

--  Tum dosya listesini, default ve fatura ozelligiyle birlikte ceker,
PROCEDURE doc_type_list (
                           p_clm_com_docs_index_tab   IN OUT clm_com_docs_index_tab,
                           p_process_results          IN OUT customer.process_result_table
                        );
-- HASAR TÜRÜME GÖRE DEFAULT GELECEK EVRAK LISTESI
--procedure  DOC_TYPE_LIST_BY_CLM ( p_EXT_REFERENCE IN varchar2, P_DOC_TYPE_LIST OUT refcur);
PROCEDURE DOC_TYPE_LIST_BY_CLM (p_EXT_REFERENCE   IN     VARCHAR2,
                                   P_CLM_COM_DOCS_INDEX_TAB   IN OUT CLM_COM_DOCS_INDEX_TAB,
                                   P_PROCESS_RESULTS          IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
                                    P_DEFAULT_DOC    IN NUMBER
                                   );
                --DOCUMENT_CODE,IS_INVOICE, ORDER_NO, DOCUMENT_NAME

/*-- GENEL EVRAK LISTESI
-- bir üstteki   DOC_TYPE_LIST_BY_CLM isimli prosedüre p_default_doc alani eklenerek iptal edildi.
 -- p_default_doc =0 ile iliskili tüm evrak listesi gelecek
procedure  DOC_TYPE_LIST (P_DOC_TYPE_LIST OUT refcur);
                --DOCUMENT_CODE,IS_INVOICE, ORDER_NO, DOCUMENT_NAME
*/


procedure doc_type_com_list_by_clm (
   p_ext_reference varchar2,
   p_communication_no number,
   p_clm_com_docs_index_tab   in out clm_com_docs_index_tab,
   p_process_results          in out customer.process_result_table

);

procedure  GET_EKSPERT_LIST_BY_CODE (P_CODE NUMBER, P_EKSPERT_LIST OUT refcur);
                            --EKSPERT_CODE,EKSPERT_NAME

procedure  GET_ASER_LIST_BY_CODE (P_CODE NUMBER, P_ASER_LIST OUT refcur);
                            --ASER_CODE,EKSPERT_NAME

procedure  GET_EKSPERT_ASER_LIST_BY_TYPE (P_CODE NUMBER, P_TYPE VARCHAR2, P_LIST OUT refcur);
                            --EKSPERT_CODE or ASER_CODE, NAME

-- ICMAL LISTESI
procedure  GET_DOC_LIST_BY_ARCHIVE_NO(P_Archive_No  IN Number, P_CLM_COM_DOCS_INDEX_TAB OUT CLM_COM_DOCS_INDEX_TAB);


--GELEN EVRAK KAYIT EKRANI EKSIK EVRAK LISTESI POPUP
procedure  GET_ABSENT_DOC (p_EXT_REFERENCE IN varchar2,P_ABSENT_DOC_LIST OUT refcur);

--gelen EVRAK KAYIRT SORGULAMA VE GÖRÜNTÜLEME evrak HAREKETLERI LISTESI popup
procedure  GET_DOC_MOVEMENTS(p_EXT_REFERENCE IN varchar2,P_DOC_MOVEMENTS_LIST OUT refcur);

-- Fatura giris ekrani, dosya sistemde varmi ?
PROCEDURE CHECK_EXT_REFERENCE (p_EXT_REFERENCE        IN     VARCHAR2,
                                p_check_ext_reference  out number,
                               p_platform in varchar2 default 'DMS' );

-- Fatura giris ekrani, dosyalar sistemde varmi ?
PROCEDURE CHECK_EXT_REFERENCE_LIST (
       p_ext_reference_list         IN     VARCHAR2,
       p_check_ext_reference_list   OUT    VARCHAR2,
       p_platform                   IN     VARCHAR2 DEFAULT 'DMS'
    );


-- indexlenmmis dosya arama da kullanilacak user adlari.

PROCEDURE GET_INDEX_USER_LIST ( P_INDEX_USER_LIST      OUT refcur);

FUNCTION GET_CLM_CANCELLED_STATUS(P_EXT_REFERENCE VARCHAR2) return number;

FUNCTION GET_CLM_CLOSED_STATUS(P_EXT_REFERENCE VARCHAR2)  RETURN NUMBER ;

PROCEDURE GET_COURT_LIST(cur                 OUT     refcur,
                         p_process_results   OUT     customer.process_result_table);

PROCEDURE DOC_TYPE_COM_LIST_BY_LAW_FILE (
   p_law_file_no              IN   VARCHAR2,
   p_communication_no         IN   NUMBER,
   p_clm_com_docs_index_tab   IN OUT clm_com_docs_index_tab,
   p_process_results          IN OUT customer.process_result_table

);

FUNCTION get_law_doc_name (p_doc_code IN VARCHAR2)
   RETURN VARCHAR2;

PROCEDURE GET_DEPT_DOC_REL(P_DEPT_DOC_REL OUT SYS_REFCURSOR);

END;
/

