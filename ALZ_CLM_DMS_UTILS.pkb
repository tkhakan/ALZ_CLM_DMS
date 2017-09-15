CREATE OR REPLACE PACKAGE BODY CUSTOMER."ALZ_CLM_DMS_UTILS"
AS
  ------- mustafa@kora
  -- log için kullanilacak, gelen degerin number veya varchar olmasina göre gerekli düzenlemeyi yapar
   Procedure Addrec(P_Tag  Varchar2
                  , P_Val  Varchar2
                  , P_Data In Out Clob)
   Is
      V_Tmp Number;
   Begin
      Begin
         if p_val is null then return; end if;

         V_Tmp := To_Number(P_Val);

         P_Data := P_Data || Chr(13) || P_Tag || ' := to_number(''' || P_Val || ''');';
      Exception When Others Then
         P_Data := P_Data || Chr(13) || P_Tag || ' := ''' || replace(P_Val, '''', '''''') || ''';';
      End;
   End;

   -- boolean ise
   Procedure Addrec(P_Val   Varchar2
                  , P_Apost Boolean Default True
                  , P_Data  In Out Clob)
   Is
      V_Tmp Number;
   Begin
      if p_val is null then return; end if;
      Begin
         If P_Apost Then
            P_Data := P_Data || Chr(13) || '''' || P_Val || ''';';
         Else
            P_Data := P_Data || Chr(13) || P_Val || ';';
         End If;
      End;
   End;
------------

/* Formatted on 27.09.2013 16:00:19 (QP5 v5.126.903.23003) */
FUNCTION GET_PLATE_NO_BY_TYPE (P_CLAIM_ID    IN NUMBER,
                               P_SF_NO       IN NUMBER,
                               P_IS_MAGDUR   IN NUMBER)
   RETURN VARCHAR2
IS
   V_RESULT   VARCHAR2 (500);
BEGIN
   IF NVL (P_IS_MAGDUR, 0) <> 0                  --  BRANCH CODE = T  ICIN -SC
   THEN
      SELECT                                     --A.CITY_PLATE_CODE || ' ' ||
            A  .PLATE_NO
        INTO   V_RESULT
        FROM   KOC_CLM_VEH_INFO A, CLM_SUBFILES B
       WHERE       A.CLAIM_ID = B.CLAIM_ID
               AND A.SF_NO = B.SF_NO
               AND B.CLAIM_ID = P_CLAIM_ID
               AND B.SF_TYPE IN ('T', 'TI')
               AND ROWNUM < 2;
   ELSE
      SELECT   A.PLATE_NO
        INTO   V_RESULT
        FROM   KOC_OCP_LAND_VEHICLES A, CLM_POL_BASES B, CLM_POL_OAR D
       WHERE       A.CONTRACT_ID = B.CONTRACT_ID
               AND D.OAR_NO = A.PARTITION_NO
               AND A.ACTION_CODE <> 'D'
               AND A.REVERSING_VERSION IS NULL
               AND B.CLAIM_ID = P_CLAIM_ID
               AND B.CLAIM_ID = D.CLAIM_ID
               AND B.CONTRACT_ID = D.CONTRACT_ID
               AND ROWNUM < 2;
   END IF;

   RETURN V_RESULT;
EXCEPTION
   WHEN OTHERS
   THEN
      RETURN NULL;
END;



FUNCTION get_doc_name (p_doc_code IN VARCHAR2)
   RETURN VARCHAR2
IS
   v_result   VARCHAR2 (500);
BEGIN
   SELECT   explanation
      INTO   v_result
     FROM   koc_clm_doc_ref a
    WHERE   ltrim(a.doc_code,'0') =ltrim(p_doc_code ,'0')
    and a.VALIDITY_END_DATE is null;


   RETURN v_result;

    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN null;

END;



FUNCTION get_max_order (p_communication_no   IN NUMBER,
                        p_archive_no         IN NUMBER,
                        p_ext_reference      IN VARCHAR2,
                        p_doc_code           IN VARCHAR2)
   RETURN NUMBER
IS
   v_result   VARCHAR2 (500);
BEGIN

   BEGIN
    SELECT MAX (order_no) order_no
        INTO v_result
      FROM alz_clm_com_docs_index_tbl a
    WHERE a.archive_no             = p_archive_no
         AND a.communication_no = p_communication_no
         AND a.ext_reference        = NVL (p_ext_reference, 'X')
         AND a.document_code     = p_doc_code;
   EXCEPTION
    WHEN OTHERS THEN
    v_result := 0;
   END;

   RETURN v_result;
END;



FUNCTION get_ext_reference_by_rucu (p_ext_reference     IN     VARCHAR2)
   RETURN VARCHAR2
IS
   v_result   VARCHAR2 (100);
BEGIN

      v_result := p_ext_reference;

      if (substr(p_ext_reference, 6, 1) = 'R') --YYYY RK NNNNNN
      then

        SELECT   clm_ext_reference
          INTO   v_result
          FROM   koc_clm_recourse_detail
         WHERE   ext_reference = p_ext_reference;

      end if;

   RETURN v_result;
END;


FUNCTION get_ext_reference_by_rucu (
                                      p_year              IN     NUMBER,
                                      p_branch            IN     VARCHAR2,
                                      p_num               IN     VARCHAR2)
   RETURN VARCHAR2
IS
   v_result   VARCHAR2 (100);
BEGIN

   v_result := get_ext_reference_by_rucu(p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM);

   RETURN v_result;
END;



---------------------------------------------------------------------------------

FUNCTION IS_DOC_INVOICE (p_DOC_CODE       IN     VARCHAR2
                                ) return number

   IS
       v_result   VARCHAR2 (500);
BEGIN
   SELECT   IS_INVOICE
     INTO   v_result
     FROM   koc_clm_doc_ref a
    WHERE   a.doc_code = p_doc_code AND ROWNUM < 2;

   RETURN NVL(v_result, 0);

    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN '0';

END;


------------------------------------------------------------------------------

PROCEDURE CHANGE_DOC_STATUS_BY_EXT_REF (p_is_insert       IN NUMBER, -- insert:1, update:0
                                        p_ext_reference   IN VARCHAR2,
                                        p_loginuser       IN VARCHAR2,
                                        p_doc_name        IN VARCHAR2)
IS
   v_claim_id   NUMBER;
   v_sf_no      NUMBER;
   v_status     VARCHAR2 (10);
   v_comment    VARCHAR2 (60);
   v_indexoftire NUMBER;
   v_ext_ref    VARCHAR2 (60);

BEGIN

    v_indexoftire :=  INSTR(p_ext_reference,'-');

       if v_indexoftire > 0 then

          v_indexoftire := v_indexoftire-1;
          v_ext_ref := SUBSTR(p_ext_reference,0,v_indexoftire);
       else
          v_ext_ref := p_ext_reference;
       end if;

   SELECT   a.claim_id, a.sf_no
     INTO   v_claim_id, v_sf_no
     FROM   koc_clm_subfiles_ext a, clm_subfiles b
    WHERE       B.ext_reference = v_ext_ref
            AND a.claim_id = b.claim_id
            AND a.sf_no = b.sf_no;


   SELECT   DECODE (p_is_insert, 1, 'DOCINSERT', 'DOCUPDATE'),
            DECODE (p_is_insert,
                    1, 'Evrak eklendi-',
                    'Evrak güncellendi-') ||substr(p_doc_name,1,25) ||'(Muhaberat)'
     INTO   v_status, v_comment
     FROM   DUAL;


   alz_expert_payment_utils.insert_claim_status (v_claim_id,
                                                 1,
                                                 'SUBFILE',
                                                 v_status,
                                                 SYSDATE,
                                                 NVL (p_loginuser, USER),
                                                 v_sf_no,
                                                 NULL,
                                                 v_comment);
END;

   PROCEDURE FIND_MANUAL_DETAIL (
      p_year              IN     NUMBER,
      p_branch            IN     VARCHAR2,
      p_num               IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      v_ext_reference VARCHAR2(100);

      CURSOR C1(pc_ext_reference VARCHAR2)
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  B.OLDSYS_POLICY_NO,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.TERM_START_DATE,
                  B.TERM_END_DATE,
                  B.POLICY_REF
           FROM   CLM_SUBFILES A, KOC_CLM_SUBFILES_EXT B, KOC_CLM_DETAIL C
          WHERE   A.EXT_REFERENCE = pc_ext_reference--p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)   -- milat tarihi belli olunca degistir ha ??
                   AND  (a.sf_type
                    in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                      or substr(sf_type,1,1) ='R');


      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      v_ext_reference := get_ext_reference_by_rucu(p_year, p_branch, p_num);
      OPEN C1(v_ext_reference);

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;

         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               NULL,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.OLDSYS_POLICY_NO,
               NULL,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      -- bu bilgilere ihtiyac olabilir mi ?
      /*p_OPUS_CLM_LIST.SF_TYPE       := C1_ROW.SF_TYPE;
      p_OPUS_CLM_LIST.CLAIM_ID      := C1_ROW.CLAIM_ID;
      p_OPUS_CLM_LIST.SF_NO         := C1_ROW.SF_NO;
      p_OPUS_CLM_LIST.LOSS_DATE     := C1_ROW.DATE_OF_LOSS ;
      p_OPUS_CLM_LIST.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      p_OPUS_CLM_LIST.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;
      p_OPUS_CLM_LIST.DENOUNCE_DATE :=  C1_ROW.DENOUNCE_DATE) ;*/


      END LOOP;

      CLOSE C1;
   END;

   ---------------------------------------------------------

   PROCEDURE FIND_VERGI_NO (
      p_tax               IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS

      CURSOR POLICE
      IS
         SELECT
               A  .CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.PART_ID in (SELECT  PART_ID
                            FROM   KOC_CP_PARTNERS_EXT
                        WHERE   TAX_NUMBER = p_TAX)
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND H.CLAIM_ID = A.CLAIM_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
        p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

         OPEN POLICE;

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;

            IF V_POLICE.EXT_REFERENCE LIKE '%/%'
            THEN
               p_OPUS_CLM_LIST.EXTEND;
               p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
                  customer.OPUS_CLM_LIST_REC (
                     V_POLICE.EXT_REFERENCE,
                     KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                     V_POLICE.TITLE,
                     get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                     V_POLICE.POLICY_REF,
                     V_POLICE.OLDSYS_POLICY_NO,
                     V_POLICE.TERM_START_DATE,
                     V_POLICE.TERM_END_DATE,
                     V_POLICE.DATE_OF_LOSS,
                     V_POLICE.DENOUNCE_DATE,
                     V_POLICE.STATUS,                -- nasil ayrilacak  ?? ha
                     V_POLICE.STATUS,                -- nasil ayrilacak  ?? ha
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                     get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                     NULL,
                     NULL,
                     NULL,
                     NULL
                  );
            ELSE
               p_OPUS_CLM_LIST.EXTEND;
               p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
                  customer.OPUS_CLM_LIST_REC (
                     V_POLICE.EXT_REFERENCE,
                     KOC_CLM_UTILS.INS_NAME_BUL (v_police.CLAIM_ID),
                     V_POLICE.TITLE,
                     NULL,
                     V_POLICE.POLICY_REF,
                     V_POLICE.OLDSYS_POLICY_NO,
                     V_POLICE.TERM_START_DATE,
                     V_POLICE.TERM_END_DATE,
                     V_POLICE.DATE_OF_LOSS,
                     V_POLICE.DENOUNCE_DATE,
                     V_POLICE.STATUS,                -- nasil ayrilacak  ?? ha
                     V_POLICE.STATUS,                -- nasil ayrilacak  ?? ha
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                     V_POLICE.PLATE_NO,
                     NULL,
                     NULL,
                     NULL,
                     NULL
                  );
            END IF;

         END LOOP;

         CLOSE POLICE;

   END;

   --------------------------------------------------------------------------------
 procedure find_tck_no (
      p_tck_no               in     varchar2,
      p_opus_clm_list        out opus_clm_list_rec_tab,
      p_process_results   in out customer.process_result_table,
      p_platform in varchar2 default 'DMS'
   )
   is
      cursor unsur_ozel
      is
         select   a  .part_id
           from   koc_cp_partners_ext a
          where   a.identity_no  = p_tck_no;

      v_unsur_o    unsur_ozel%rowtype;
      v_part_id    number;

      cursor police (
         p_part number
      )
      is
         select
               distinct a  .claim_id,
                  a.sf_no,
                  f.contract_id,
                  f.version_no,
                  a.ext_reference,
                  b.date_of_loss,
                  c.denounce_date,
                  g.reference_code,
                  decode (a.clm_status,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     status,
                  e.title,
                  b.oldsys_policy_no,
                  f.policy_ref,
                  f.term_start_date,
                  f.term_end_date,
                  decode (b.is_industrial, 1, 'E', 'B') industrial,
                  a.sf_type,
                  c.denouncer_name,
                  b.plate_no
           from   clm_subfiles a,
                  koc_clm_subfiles_ext b,
                  koc_clm_detail c,
                  clm_pol_bases f,
                  dmt_agents g,
                  koc_dmt_agents_ext e,
                  clm_interested_parties h
          where       h.part_id = p_part
                  and a.claim_id = b.claim_id
                  and a.sf_no = b.sf_no
                  and b.claim_id = f.claim_id
                  and b.claim_id = c.claim_id
                  and b.sf_no = c.sf_no
                  and f.agent_role = g.int_id
                  and g.int_id = e.int_id
                  and h.claim_id = a.claim_id
                  and c.denounce_date>=alz_clm_dms_main_utils.getmilatdate(a.sf_type, p_platform)
                   and  (a.sf_type in ( select a.explanation  from koc_clm_lookup a
                         where type = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      v_police     police%rowtype;
      v_status     number;
      v_messages   varchar2 (100);
   begin
      p_opus_clm_list := opus_clm_list_rec_tab ();

      open unsur_ozel;

      loop
         fetch unsur_ozel into v_unsur_o;

         exit when unsur_ozel%notfound;

         open police (v_unsur_o.part_id);

         loop
            fetch police into v_police;

            exit when police%notfound;

            if v_police.ext_reference like '%/%'
            then
               p_opus_clm_list.extend;
               p_opus_clm_list (p_opus_clm_list.count) :=
                  customer.opus_clm_list_rec (
                     v_police.ext_reference,
                     koc_clm_utils.ins_name_bul (v_police.claim_id),
                     v_police.title,
                     get_plate_no_by_type (v_police.claim_id, v_police.sf_no, 0),
                     v_police.policy_ref,
                     v_police.oldsys_policy_no,
                     v_police.term_start_date,
                     v_police.term_end_date,
                     v_police.date_of_loss,
                     v_police.denounce_date,
                     v_police.status,                -- nasil ayrilacak  ?? ha
                     v_police.status,                -- nasil ayrilacak  ?? ha
                     null,
                     null,
                     null,
                     null,
                     null,
                     null,
                     null,
                     koc_clm_utils.mdr_name_bul (v_police.claim_id, v_police.sf_no),
                     get_plate_no_by_type (v_police.claim_id, v_police.sf_no, 1),
                     null,
                     null,
                     null,
                     null
                  );
            else
               p_opus_clm_list.extend;
               p_opus_clm_list (p_opus_clm_list.count) :=
                  customer.opus_clm_list_rec (
                     v_police.ext_reference,
                     koc_clm_utils.ins_name_bul (v_police.claim_id),
                     v_police.title,
                     get_plate_no_by_type (v_police.claim_id, v_police.sf_no, 0),
                     v_police.policy_ref,
                     v_police.oldsys_policy_no,
                     v_police.term_start_date,
                     v_police.term_end_date,
                     v_police.date_of_loss,
                     v_police.denounce_date,
                     v_police.status,                -- nasil ayrilacak  ?? ha
                     v_police.status,                -- nasil ayrilacak  ?? ha
                     null,
                     null,
                     null,
                     null,
                     null,
                     null,
                     null,
                     koc_clm_utils.mdr_name_bul (v_police.claim_id, v_police.sf_no),
                     get_plate_no_by_type (v_police.claim_id, v_police.sf_no, 1),
                     null,
                     null,
                     null,
                     null
                  );
            end if;
         /*    -- bu bilgilere ihtiyac olabilir mi ?
     p_opus_clm_list.sf_type       := v_police.sf_type;
     p_opus_clm_list.claim_id      := v_police.claim_id;
     p_opus_clm_list.sf_no         := v_police.sf_no;
     p_opus_clm_list.contract_id   := v_police.contract_id;
     p_opus_clm_list.version_no    := v_police.version_no;
     p_opus_clm_list.reference_code:= v_police.reference_code;
     p_opus_clm_list.industrial      :=  v_police.industrial ;
      p_opus_clm_list.denouncer_name := v_police.denouncer_name;
 */



         end loop;


         close police;
      end loop;

      close unsur_ozel;
   end;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_MONDIAL (
      p_mondial_no        IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       B.CLAIM_ID > 0
                  AND B.SF_NO > 0
                  AND B.MONDEAL_NO = p_mondial_no
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --    :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --    :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --    :DETAIL.SF_NO         := C1_ROW.SF_NO;
      --    :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --    :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      --     :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      --:DETAIL.STATUS          := C1_ROW.STATUS;
      --    :MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;


      END LOOP;

      CLOSE C1;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_TRAMER_DETAIL (
      p_tramer_ihbar_no   IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       C.TRMER_CLAIM_NO = p_TRAMER_IHBAR_NO
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --:DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --:DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --:DETAIL.SF_NO         := C1_ROW.SF_NO;
      --:DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --:DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      -- :DETAIL.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      -- :MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;

      --  BEGIN
      --     SELECT OAR_NO INTO :DETAIL.OAR_NO
      --     FROM CLM_POL_OAR
      --  WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
      --  EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
      -- END;

      END LOOP;

      CLOSE C1;
   END;

   ---------------------------------------------------------------------------------

   PROCEDURE FIND_SZ (
      p_sozlesme_no        IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  koc_ocp_pol_contracts_ext S
          WHERE       B.CLAIM_ID > 0
                  AND B.SF_NO > 0
                  AND S.LEASING_NUMBER = p_sozlesme_no --LEASING_NUMBER ile mondial no anlamli mi ??  ha
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND F.CONTRACT_ID = S.CONTRACT_ID
                  and C.DENOUNCE_DATE>= ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,                --COMMUNICATION_NO
               NULL,                --ARCHIVE_NO
               NULL,                --INDEX_DATE
               NULL,                --DEPARTMENT_NAME
               NULL,                --COMM_USER_NAME
               NULL,                --EXPLANATION
               NULL,                --DOC_SENDER
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --    :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --   :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --  :DETAIL.SF_NO         := C1_ROW.SF_NO;
      --  :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --  :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      --:DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      --:MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;




      END LOOP;

      CLOSE C1;
   END;

   ---------------------------------------------------------------------------------

   PROCEDURE FIND_POLICY_DETAIL (
      p_old_pol           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  C.DENOUNCER_SURNAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       B.OLDSYS_POLICY_NO = p_old_pol
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();


      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      -- :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --:DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --:DETAIL.SF_NO         := C1_ROW.SF_NO;
      --:DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --:DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      :=  C1_ROW.INDUSTRIAL ;
      --:MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME||' '||C1_ROW.DENOUNCER_SURNAME;



      END LOOP;

      CLOSE C1;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_FILE_DETAIL (
      p_year              IN     NUMBER,
      p_branch            IN     VARCHAR2,
      p_num               IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform  IN VARCHAR2 default 'DMS'
   )
   IS
      v_ext_reference VARCHAR2(100);

      CURSOR C1(pc_ext_reference VARCHAR2)
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  c.DENOUNCER_SURNAME
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE   A.EXT_REFERENCE = pc_ext_reference
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>= ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      v_ext_reference := get_ext_reference_by_rucu(p_year, p_branch, p_num);
      OPEN C1(v_ext_reference);

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM,
               substr(KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),1,100),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      -- :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      -- :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      -- :DETAIL.SF_NO         := C1_ROW.SF_NO;
      -- :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      -- :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      -- :MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME||' '||C1_ROW.DENOUNCER_SURNAME;


      /* BEGIN
            SELECT OAR_NO INTO :DETAIL.OAR_NO
              FROM CLM_POL_OAR
             WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
         EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
       END;
    */

      END LOOP;

      CLOSE C1;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_SAGLIK_BAKANLIGI (
      p_invoice_no        IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       C.INVOICE_NO = p_INVOICE_NO
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform ) or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --    :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --    :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --    :DETAIL.SF_NO         := C1_ROW.SF_NO;
      --    :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --    :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      --    :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --  :DETAIL.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      --    :MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;

      /*  BEGIN
             SELECT OAR_NO INTO :DETAIL.OAR_NO
               FROM CLM_POL_OAR
              WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
          EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
        END;*/


      END LOOP;

      CLOSE C1;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_PLATE_DETAIL (
      p_plate_no          IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C2
      IS
         SELECT   b.claim_id,
                  b.sf_no,
                  ext_reference,
                  A.PLATE_NO
           FROM   koc_clm_veh_info a, clm_subfiles b
          WHERE       a.claim_id = b.claim_id
                  AND a.sf_no = b.sf_no
                  AND a.plate_no = p_PLATE_NO
         UNION
         SELECT   b.claim_id,
                  c.sf_no,
                  ext_reference,
                  A.PLATE_NO
           FROM   koc_ocp_land_vehicles a,
                  clm_pol_bases b,
                  clm_subfiles c,
                  CLM_POL_OAR D
          WHERE       a.contract_id = b.contract_id
                  AND a.plate_no = p_PLATE_NO
                  AND D.CLAIM_ID = C.CLAIM_ID
                  AND D.OAR_NO = A.PARTITION_NO
                  AND a.action_code <> 'D'
                  AND a.reversing_version IS NULL
                  AND a.top_indicator = 'Y'
                  AND b.claim_id = c.claim_id;


      C2_ROW                C2%ROWTYPE;


      CURSOR C1 (
         p_claim_id                 NUMBER,
         p_sf_no                    NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  --F.CONTRACT_ID,
                  -- F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  --   G.REFERENCE_CODE,
                  --     C.DENOUNCER_NAME ,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,                         -- nasil ayrilacak  ?? ha
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     doc_STATUS,                     -- nasil ayrilacak  ?? ha
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  B.PLATE_NO
           --  DECODE(B.IS_INDUSTRIAL,1,'E','B') INDUSTRIAL
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       A.CLAIM_ID = p_CLAIM_ID
                  AND A.SF_NO = p_SF_NO
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                   AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW                C1%ROWTYPE;

      V_status              NUMBER;
      V_messages            VARCHAR2 (100);

      v_OPUS_CLM_LIST_REC   customer.OPUS_CLM_LIST_REC;
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C2;

      LOOP
         FETCH C2 INTO C2_ROW;

         EXIT WHEN C2%NOTFOUND;

         OPEN C1 (C2_ROW.CLAIM_ID, C2_ROW.SF_NO);

         LOOP
            FETCH C1 INTO C1_ROW;

            EXIT WHEN C1%NOTFOUND;

            --:DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
            --:DETAIL.SF_NO         := C1_ROW.SF_NO;

            p_OPUS_CLM_LIST.EXTEND;


            IF C1_ROW.EXT_REFERENCE LIKE '%/%'
            THEN
               p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
                  customer.OPUS_CLM_LIST_REC (
                     C1_ROW.EXT_REFERENCE,
                     KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
                     C1_ROW.TITLE,
                     get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
                     C1_ROW.POLICY_REF,
                     C1_ROW.OLDSYS_POLICY_NO,
                     C1_ROW.TERM_START_DATE,
                     C1_ROW.TERM_END_DATE,
                     C1_ROW.DATE_OF_LOSS,
                     C1_ROW.DENOUNCE_DATE,
                     C1_ROW.STATUS,                  -- nasil ayrilacak  ?? ha
                     C1_ROW.STATUS,                  -- nasil ayrilacak  ?? ha
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
                     get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
                     NULL,
                     NULL,
                     NULL,
                     NULL
                  );
            ELSE
               p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
                  customer.OPUS_CLM_LIST_REC (
                     C1_ROW.EXT_REFERENCE,
                     KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
                     C1_ROW.TITLE,
                     get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
                     C1_ROW.POLICY_REF,
                     C1_ROW.OLDSYS_POLICY_NO,
                     C1_ROW.TERM_START_DATE,
                     C1_ROW.TERM_END_DATE,
                     C1_ROW.DATE_OF_LOSS,
                     C1_ROW.DENOUNCE_DATE,
                     C1_ROW.STATUS,                  -- nasil ayrilacak  ?? ha
                     C1_ROW.STATUS,                  -- nasil ayrilacak  ?? ha
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     NULL,
                     KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
                     get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
                     NULL,
                     NULL,
                     NULL,
                     NULL
                  );
            END IF;
         /*BEGIN
        SELECT OAR_NO INTO :DETAIL.OAR_NO
        FROM CLM_POL_OAR
        WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
        EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
        END;*/


         END LOOP;

         CLOSE C1;
      END LOOP;

      CLOSE C2;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_INS_person_DETAIL (
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A.PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_surname || '%'
                  AND A.FIRST_NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'INS'
                  and C.DENOUNCE_DATE>= ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();


      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;

            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --  :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         -- :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --:DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         --   :MOTOR_DETAIL.DENOUNCER_NAME := V_POLICE.DENOUNCER_NAME;

         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------
PROCEDURE FIND_INSD_person_DETAIL (
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A.PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_surname || '%'
                  AND A.FIRST_NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'PH'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();


      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;

            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --  :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         -- :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --:DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         --   :MOTOR_DETAIL.DENOUNCER_NAME := V_POLICE.DENOUNCER_NAME;

         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------


PROCEDURE FIND_mdr_person_DETAIL (
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS


      CURSOR UNSUR_OZEL
      IS
         SELECT
               A  .PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_surname || '%'
                  AND A.FIRST_NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM
                   clm_interested_parties P,
                  CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE
                  p.IP_TYPE = 'MDR' and
                  p.CLAIM_ID > 0
                  AND p.IP_NO > 0
                  AND p.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = p.CLAIM_ID
                  --AND p.IP_TYPE = 'PH'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();


      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;

            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --  :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         -- :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --:DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         --   :MOTOR_DETAIL.DENOUNCER_NAME := V_POLICE.DENOUNCER_NAME;

         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;


------------------------------------------------------------------------------------
   PROCEDURE FIND_INS_INST_DETAIL (
      p_name              IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
                 A.PART_ID, a.name
           FROM  CP_PARTNERS A
          WHERE  A.NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'INS'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;
            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --     :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         --     :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         --     :DETAIL.SF_NO         := V_POLICE.SF_NO;
         --    :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         --    :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         --     :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --    :DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         --   :MOTOR_DETAIL.DENOUNCER_NAME := V_POLICE.DENOUNCER_NAME;


         /* BEGIN
        SELECT OAR_NO INTO :DETAIL.OAR_NO
        FROM CLM_POL_OAR
        WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
        EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
        END;*/


         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------
PROCEDURE FIND_INSD_INST_DETAIL (
      p_name              IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A.PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'PH'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;
            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --     :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         --     :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         --     :DETAIL.SF_NO         := V_POLICE.SF_NO;
         --    :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         --    :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         --     :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --    :DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         --   :MOTOR_DETAIL.DENOUNCER_NAME := V_POLICE.DENOUNCER_NAME;


         /* BEGIN
        SELECT OAR_NO INTO :DETAIL.OAR_NO
        FROM CLM_POL_OAR
        WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
        EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
        END;*/


         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_PH_person_DETAIL (
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A.PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_SURname || '%'
                  AND A.FIRST_NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'PH'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();


      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;
            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --    :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         --  :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --:DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;

         END LOOP;


         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_PH_INST_DETAIL (
      p_name              IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A.PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_NAME || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'PH'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;
            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --   :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         -- :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         -- :DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;

         /* BEGIN
         SELECT OAR_NO INTO :DETAIL.OAR_NO
         FROM CLM_POL_OAR
         WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
         EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
         END;*/


         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   -----------YENI YAZDIM GEREK YOK ---------------------------------------------------------------------
/*
   PROCEDURE FIND_MDR_person_DETAIL (
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A  .PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_surname || '%'
                  AND A.FIRST_NAME LIKE p_name || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'MDR'
                  and C.DENOUNCE_DATE>=to_date('01/01/2012')
                  AND  (a.sf_type in (
                    'K',
                    'T',
                    'TI',
                    'C',
                    'MX',
                    'TS',
                    'HK',
                    'FCR'
                    ) or substr(sf_type,1,1) ='R');

      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;
            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  NVL (
                     KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID,
                                                 V_POLICE.SF_NO),
                     KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID)
                  ),
                  V_POLICE.TITLE,
                  NULL,
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         -- :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         --  :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         -- :DETAIL.SF_NO         := V_POLICE.SF_NO;
         -- :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         -- :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         -- :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         -- :DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;

         END LOOP;


         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;
*/
   --------------------------------------------------------------------------------
   PROCEDURE FIND_MDR_INST_DETAIL (
      p_name              IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
        )
   IS
      CURSOR UNSUR_OZEL
      IS
         SELECT
               A  .PART_ID, a.name
           FROM   CP_PARTNERS A
          WHERE   A.NAME LIKE p_NAME || '%';

      V_UNSUR_O    UNSUR_OZEL%ROWTYPE;
      V_PART_ID    NUMBER;

      CURSOR POLICE (
         P_PART NUMBER
      )
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E,
                  CLM_INTERESTED_PARTIES H
          WHERE       H.CLAIM_ID > 0
                  AND H.IP_NO > 0
                  AND H.PART_ID = P_PART
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND A.CLAIM_ID = H.CLAIM_ID
                  AND H.IP_TYPE = 'MDR'
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');


      V_POLICE     POLICE%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN UNSUR_OZEL;

      LOOP
         FETCH UNSUR_OZEL INTO V_UNSUR_O;

         EXIT WHEN UNSUR_OZEL%NOTFOUND;

         OPEN POLICE (V_UNSUR_O.PART_ID);

         LOOP
            FETCH POLICE INTO V_POLICE;

            EXIT WHEN POLICE%NOTFOUND;

            p_OPUS_CLM_LIST.EXTEND;
            p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
               customer.OPUS_CLM_LIST_REC (
                  V_POLICE.EXT_REFERENCE,
                  KOC_CLM_UTILS.INS_NAME_BUL (V_POLICE.CLAIM_ID),
                  V_POLICE.TITLE,
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 0),
                  V_POLICE.POLICY_REF,
                  V_POLICE.OLDSYS_POLICY_NO,
                  V_POLICE.TERM_START_DATE,
                  V_POLICE.TERM_END_DATE,
                  V_POLICE.DATE_OF_LOSS,
                  V_POLICE.DENOUNCE_DATE,
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  V_POLICE.STATUS,                   -- nasil ayrilacak  ?? ha
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  KOC_CLM_UTILS.MDR_NAME_BUL (V_POLICE.CLAIM_ID, V_POLICE.SF_NO),
                  get_plate_no_by_type (V_POLICE.CLAIM_ID, V_POLICE.SF_NO, 1),
                  NULL,
                  NULL,
                  NULL,
                  NULL
               );
         --  :DETAIL.SF_TYPE       := V_POLICE.SF_TYPE;
         --  :DETAIL.CLAIM_ID      := V_POLICE.CLAIM_ID;
         --  :DETAIL.SF_NO         := V_POLICE.SF_NO;
         --  :DETAIL.CONTRACT_ID   := V_POLICE.CONTRACT_ID;
         --  :DETAIL.VERSION_NO    := V_POLICE.VERSION_NO;
         --  :DETAIL.REFERENCE_CODE:= V_POLICE.REFERENCE_CODE;
         --  :DETAIL.INDUSTRIAL      :=  V_POLICE.INDUSTRIAL ;
         /* BEGIN
      SELECT OAR_NO INTO :DETAIL.OAR_NO
      FROM CLM_POL_OAR
      WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
      EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
      END;*/

         END LOOP;

         CLOSE POLICE;
      END LOOP;

      CLOSE UNSUR_OZEL;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_POL_DETAIL (
      p_contract_id       IN     NUMBER,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  C.DENOUNCER_SURNAME,
                  O.OAR_NO,                                           --BKUNDAK
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  clm_pol_oar O,                                     --BKUNDAK
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       F.contract_id = p_contract_id
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND b.claim_id = O.claim_id                        --BKUNDAK
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;

      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;

         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      -- :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --:DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --:DETAIL.SF_NO         := C1_ROW.SF_NO;
      --:DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --:DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      --:DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      :=  C1_ROW.INDUSTRIAL ;
      --:DETAIL.OAR_NO                  := C1_ROW.OAR_NO; --BKUNDAK



      END LOOP;

      CLOSE C1;
   END;

   --------------------------------------------------------------------------------

   PROCEDURE FIND_FILE_AGENT_DETAIL (
      p_CONTRACT_ID       IN     NUMBER,
      p_reference_code    IN     VARCHAR2,
      p_AGENT_ID          IN     NUMBER,
      p_old_pol           IN     VARCHAR2,
      p_policy_no         IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  O.OAR_NO,                                -- BKUNDAK TASK 8718
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  clm_pol_oar O,                                     --BKUNDAK
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND b.claim_id = O.claim_id                        --BKUNDAK
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                   AND ((p_OLD_POL IS NOT NULL AND B.OLDSYS_POLICY_NO = p_OLD_POL AND F.AGENT_ROLE = p_AGENT_ID) OR
                   (p_POLICY_NO IS NOT  NULL AND F.CONTRACT_ID = p_CONTRACT_ID AND F.AGENT_ROLE = p_AGENT_ID))
                    and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      --  AND ((p_OLD_POL IS NOT NULL AND B.OLDSYS_POLICY_NO = p_OLD_POL AND F.AGENT_ROLE = p_AGENT_ID) OR
      --     (p_POLICY_NO IS NOT  NULL AND F.CONTRACT_ID = p_CONTRACT_ID AND F.AGENT_ROLE = p_AGENT_ID));
      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --:DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --:DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --:DETAIL.SF_NO         := C1_ROW.SF_NO;
      --DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --:DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      -- :DETAIL.INDUSTRIAL      :=  C1_ROW.INDUSTRIAL ;
      --  :DETAIL.OAR_NO                  := C1_ROW.OAR_NO; --BKUNDAK TASK 8718
      -- :DETAIL.INS_NAME        :=KOC_CLM_UTILS.INS_NAME_BUL(:DETAIL.CLAIM_ID);
      --:MOTOR_DETAIL.DENOUNCER_NAME  :=C1_ROW.DENOUNCER_NAME;

        /*
   BEGIN
SELECT OAR_NO INTO :DETAIL.OAR_NO
FROM CLM_POL_OAR
WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
END;*/


      END LOOP;

      CLOSE C1;
   END;

   ---------------------------------------------------------------------------------


   PROCEDURE FIND_FILE_TT_DETAIL (
      p_abone_no          IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  O.OAR_NO,                                -- BKUNDAK TASK 8718
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  clm_pol_oar O,                                     --BKUNDAK
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND b.claim_id = O.claim_id                        --BKUNDAK
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  AND EXISTS
                        (SELECT   1
                           FROM   koc_ocp_pol_versions_ext
                          WHERE   contract_id = f.contract_id
                                  AND AGENT_CUSTOMER_NO = p_abone_no)
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');

      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      -- :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      -- :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      -- :DETAIL.SF_NO         := C1_ROW.SF_NO;
      -- :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      -- :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      :=  C1_ROW.INDUSTRIAL ;
      -- :DETAIL.OAR_NO                  := C1_ROW.OAR_NO; --BKUNDAK TASK 8718
      -- :MOTOR_DETAIL.DENOUNCER_NAME  :=C1_ROW.DENOUNCER_NAME;

      /*  BEGIN
     SELECT OAR_NO INTO :DETAIL.OAR_NO
     FROM CLM_POL_OAR
     WHERE CLAIM_ID= :DETAIL.CLAIM_ID;
     EXCEPTION WHEN OTHERS THEN :DETAIL.OAR_NO:=NULL;
     END;*/

      END LOOP;

      CLOSE C1;
   END;

   ------------------------------------------------------------------------------

   PROCEDURE FIND_KARSI_SIRKET (
      p_k_dosya           IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform IN VARCHAR2 default 'DMS'
   )
   IS
      CURSOR C1
      IS
         SELECT   A.CLAIM_ID,
                  A.SF_NO,
                  F.CONTRACT_ID,
                  F.VERSION_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  G.REFERENCE_CODE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  E.TITLE,
                  B.OLDSYS_POLICY_NO,
                  F.POLICY_REF,
                  F.TERM_START_DATE,
                  F.TERM_END_DATE,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.PLATE_NO
           FROM   CLM_SUBFILES A,
                  KOC_CLM_SUBFILES_EXT B,
                  KOC_CLM_DETAIL C,
                  CLM_POL_BASES F,
                  DMT_AGENTS G,
                  KOC_DMT_AGENTS_EXT E
          WHERE       C.RECOURSE_FILE_NO = p_K_DOSYA
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = F.CLAIM_ID
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO
                  AND F.AGENT_ROLE = G.INT_ID
                  AND G.INT_ID = E.INT_ID
                  and C.DENOUNCE_DATE>=ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE(A.sf_type, p_platform)
                  AND  (a.sf_type in ( SELECT a.explanation  FROM KOC_CLM_LOOKUP A
                         WHERE TYPE = 206 and scode=p_platform )
                          or substr(sf_type,1,1) ='R');




      C1_ROW       C1%ROWTYPE;
      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
   BEGIN
      p_OPUS_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;
         p_OPUS_CLM_LIST.EXTEND;
         p_OPUS_CLM_LIST (p_OPUS_CLM_LIST.COUNT) :=
            customer.OPUS_CLM_LIST_REC (
               C1_ROW.EXT_REFERENCE,
               KOC_CLM_UTILS.INS_NAME_BUL (C1_ROW.CLAIM_ID),
               C1_ROW.TITLE,
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 0),
               C1_ROW.POLICY_REF,
               C1_ROW.OLDSYS_POLICY_NO,
               C1_ROW.TERM_START_DATE,
               C1_ROW.TERM_END_DATE,
               C1_ROW.DATE_OF_LOSS,
               C1_ROW.DENOUNCE_DATE,
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               C1_ROW.STATUS,                        -- nasil ayrilacak  ?? ha
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               KOC_CLM_UTILS.MDR_NAME_BUL (C1_ROW.CLAIM_ID, C1_ROW.SF_NO),
               get_plate_no_by_type (C1_ROW.CLAIM_ID, C1_ROW.SF_NO, 1),
               NULL,
               NULL,
               NULL,
               NULL
            );
      --  :DETAIL.SF_TYPE       := C1_ROW.SF_TYPE;
      --  :DETAIL.CLAIM_ID      := C1_ROW.CLAIM_ID;
      --  :DETAIL.SF_NO         := C1_ROW.SF_NO;
      --  :DETAIL.CONTRACT_ID   := C1_ROW.CONTRACT_ID;
      --  :DETAIL.VERSION_NO    := C1_ROW.VERSION_NO;
      -- :DETAIL.REFERENCE_CODE:= C1_ROW.REFERENCE_CODE;
      --:DETAIL.INDUSTRIAL      := C1_ROW.INDUSTRIAL ;
      --:MOTOR_DETAIL.DENOUNCER_NAME := C1_ROW.DENOUNCER_NAME;

      END LOOP;

      CLOSE C1;
   END;

   -------------------------------------------------------------------------------
   PROCEDURE GET_OPUS_CLM_LIST (
      p_criteria_type     IN     VARCHAR2,
      p_year              IN     NUMBER,
      p_branch            IN     VARCHAR2,
      p_num               IN     VARCHAR2,
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_person_type       IN     VARCHAR2,                  -- i tüzel ,p özel
      p_invoice_no        IN     VARCHAR2,
      p_rim_no            IN     VARCHAR2,
      p_reference_code    IN     VARCHAR2,                          /* acente*/
      p_old_pol           IN     VARCHAR2,
      p_policy_no         IN     VARCHAR2,
      p_plate_no          IN     VARCHAR2,
      p_k_dosya           IN     VARCHAR2,
      p_tax               IN     VARCHAR2,
      p_tck               IN     VARCHAR2,  -- 'T'
      p_tramer_ihbar_no   IN     VARCHAR2,
      p_mondial_no        IN     VARCHAR2,
      p_sozlesme_no       IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform  IN VARCHAR2 -- default 'DMS'
   )
   IS
      l_return            VARCHAR2 (5);
      V_PART_ID           PME_BV_INTERESTED_PARTIES.PARTNER_ID%TYPE;
      campaign_tbl_yeni   koc_campaign_utils.campaign_text_tbl;

      v_claim_id          NUMBER (10);
      v_sf_no             NUMBER (4);
      v_is_3h             NUMBER (1);
      v_ext_reference     VARCHAR2(100);
      v_claim_status VARCHAR2(1000);
      v_ind                  NUMBER(6);


      TYPE detail_type IS RECORD
      (
         v_manual        VARCHAR2 (30),
         PART_TYPE       VARCHAR2 (30),
         ext_reference   VARCHAR2 (30)
      );

      detail              detail_type;

      v_AGENT_ID          DMT_AGENTS.INT_ID%TYPE;
   BEGIN
      v_ext_reference := get_ext_reference_by_rucu(p_year || ' ' || p_BRANCH || ' ' || p_NUM);
      BEGIN
         SELECT   a.manual_claim,
                  A.PARTITION_TYPE,
                  a.claim_id,
                  a.sf_no
           INTO   detail.v_manual,
                  DETAIL.PART_TYPE,
                  v_claim_id,
                  v_sf_no
           FROM   koc_clm_subfiles_ext a, clm_subfiles b
          WHERE   B.ext_reference = v_ext_reference
                  AND a.claim_id = b.claim_id
                  AND a.sf_no = b.sf_no;
      EXCEPTION
         WHEN OTHERS
         THEN
            detail.v_manual := 'N';
      END;

      --
      IF detail.v_manual = 'Y'
      THEN
         alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'INVALID_DATA',
            'Poliçe kaydi sistemde yoktur.',
            'Poliçe kaydi sistemde yoktur.',
            NULL,
            NULL,
            'ALZ_CLM_DMS_UTILS , detail.v_manual ',
            NULL,
            p_process_results
         );

         FIND_MANUAL_DETAIL (p_year,
                             p_branch,
                             p_num,
                             p_OPUS_CLM_LIST,
                             P_PROCESS_RESULTS,
                             p_platform );
      ELSE
         IF p_criteria_type = 'V'
         THEN
            FIND_VERGI_NO (p_tax, p_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );
       ELSIF p_criteria_type = 'T'
         THEN
            FIND_TCK_NO (p_tck, p_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );

       ELSIF p_criteria_type = 'M'
         THEN
            FIND_MONDIAL (p_mondial_no, p_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'Z'
         THEN
            FIND_TRAMER_DETAIL (p_tramer_ihbar_no,
                                p_OPUS_CLM_LIST,
                                P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'SZ'
         THEN
            FIND_SZ (p_sozlesme_no, p_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'P'
         THEN
            FIND_POLICY_DETAIL (p_old_pol,
                                p_OPUS_CLM_LIST,
                                P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'F'
         THEN
            FIND_FILE_DETAIL (p_year,
                              p_branch,
                              p_num,
                              p_OPUS_CLM_LIST,
                              P_PROCESS_RESULTS,
                              p_platform  );

         ELSIF p_criteria_type = 'SA'
         THEN
            FIND_SAGLIK_BAKANLIGI (p_invoice_no,
                                   p_OPUS_CLM_LIST,
                                   P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'N'
         THEN
            -->-- saim kuru, kora,  12/07/2007 ---------
            -- pr_warn_if_plate_in_mdk;  -- kapattim gerekli mi ?? ha
            --<-- saim kuru, kora,  12/07/2007 ---------

            FIND_PLATE_DETAIL (p_plate_no,
                               p_OPUS_CLM_LIST,
                               P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'I'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_INS_PERSON_DETAIL (p_name,
                                       p_surname,
                                       p_OPUS_CLM_LIST,
                                       P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_INS_INST_DETAIL (p_name,
                                     p_OPUS_CLM_LIST,
                                     P_PROCESS_RESULTS,
                             p_platform );
            END IF;
         ELSIF p_criteria_type = 'IP'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_PH_PERSON_DETAIL (p_name,
                                      p_surname,
                                      p_OPUS_CLM_LIST,
                                      P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_PH_INST_DETAIL (p_name,
                                    p_OPUS_CLM_LIST,
                                    P_PROCESS_RESULTS,
                             p_platform );
            END IF;
         ELSIF p_criteria_type = 'MD'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_MDR_PERSON_DETAIL (p_name,
                                       p_surname,
                                       p_OPUS_CLM_LIST,
                                       P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_MDR_INST_DETAIL (p_name,
                                     p_OPUS_CLM_LIST,
                                     P_PROCESS_RESULTS,
                             p_platform );
            END IF;
         ELSIF p_criteria_type = 'A'
         THEN
            IF p_POLICY_NO IS NOT NULL
            THEN
               DECLARE
                  V_CONTRACT_ID   NUMBER;
               BEGIN
                  SELECT   CONTRACT_ID
                    INTO   V_CONTRACT_ID
                    FROM   OCP_POLICY_BASES
                   WHERE   POLICY_rEF = p_POLICY_NO AND ROWNUM < 2;

                  FIND_POL_DETAIL (V_CONTRACT_ID,
                                   p_OPUS_CLM_LIST,
                                   P_PROCESS_RESULTS,
                             p_platform );
               END;
            ELSE
               DECLARE
                  V_CONTRACT_ID   NUMBER;
               BEGIN
                  SELECT   CONTRACT_ID
                    INTO   V_CONTRACT_ID
                    FROM   OCP_POLICY_BASES
                   WHERE   POLICY_rEF = p_POLICY_NO AND ROWNUM < 2;

                  BEGIN
                     SELECT   INT_ID
                       INTO   v_AGENT_ID
                       FROM   DMT_AGENTS
                      WHERE   REFERENCE_CODE = p_REFERENCE_CODE;
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        alz_web_process_utils.process_result (
                           0,
                           9,
                           -1,
                           'INVALID_DATA',
                           'Partaj bulunamadi.',
                           'Partaj bulunamadi.',
                           NULL,
                           NULL,
                           'ALZ_CLM_DMS_UTILS , p_REFERENCE_CODE ',
                           NULL,
                           p_process_results
                        );
                  --   p_REFERENCE_CODE :=NULL;
                  END;


                  FIND_FILE_AGENT_DETAIL (V_CONTRACT_ID,
                                          p_reference_code,
                                          v_AGENT_ID,
                                          p_old_pol,
                                          p_policy_no,
                                          p_OPUS_CLM_LIST,
                                          P_PROCESS_RESULTS,
                             p_platform );
               END;
            END IF;
         ELSIF p_criteria_type = 'TT'
         THEN
            FIND_FILE_TT_DETAIL (p_OLD_POL,
                                 p_OPUS_CLM_LIST,
                                 P_PROCESS_RESULTS,
                             p_platform );
         ELSIF p_criteria_type = 'RM'
         THEN
            DECLARE
               V_CONTRACT_ID   NUMBER;
            BEGIN
               BEGIN
                  SELECT   contract_id
                    INTO   V_CONTRACT_ID
                    FROM   koc_ocp_pol_versions_ext
                   WHERE   agent_customer_no = p_rim_no;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     V_CONTRACT_ID := NULL;
               END;

               IF v_contract_id IS NOT NULL
               THEN
                  FIND_POL_DETAIL (V_CONTRACT_ID,
                                   p_OPUS_CLM_LIST,
                                   P_PROCESS_RESULTS,
                             p_platform );
               END IF;
            END;
         ELSIF p_criteria_type = 'K'
         THEN
            FIND_KARSI_SIRKET (p_k_dosya, p_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );
         END IF;
      END IF;


      /*  ne olmali ?? ha
     IF NVL(DETAIL.PART_TYPE,'X') ='325' THEN
         DETAIL.INS_NAME:=GET_PH_NAMEFormula;
     END IF;*/

      --more_than_denounce;   ne olacak ?? ha

      /*
      IF DETAIL.EXT_REFERENCE LIKE '% FCR %' THEN

      IF NOT KOC_AUTH_UTILS.IS_USER_AUTHORIZED(:BOILER.USERID,'FCR') THEN
      GO_BLOCK('DETAIL');
          Clear_BLOCK(No_Validate);
          GO_BLOCK('MOTOR_DETAIL');
          Clear_BLOCK(No_Validate);

      KOC_SET_ITEM('MOTOR_DETAIL.ARASTIRMA',FALSE);

         IF :MOTOR_INS_VEHICLES.PLATE_NO IS NOT NULL THEN
          go_block('MOTOR_INS_VEHICLES');
          Clear_BLOCK(No_Validate);
         END IF;
       END IF;
       GO_ITEM('DETAIL.EXT_REFERENCE');
      END IF;

      */
      IF p_OPUS_CLM_LIST is null or p_OPUS_CLM_LIST.COUNT = 0
      THEN
         alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'INVALID_DATA',
            'Girdiginiz arama kriterine ait sistemde açilmis dosya bulunmamaktadir.',
            'Girdiginiz arama kriterine ait sistemde açilmis dosya bulunmamaktadir.',
            NULL,
            NULL,
            'ALZ_CLM_DMS_UTILS , p_OPUS_CLM_LIST.count ',
            NULL,
            p_process_results
         );
      ELSE

          v_ind := p_opus_clm_list.first;

          while v_ind is not null
          loop

              alz_crm_clm_utils.get_claim_file_status(p_opus_clm_list(v_ind).ext_reference , v_claim_status);

              p_opus_clm_list(v_ind).claim_status  := v_claim_status;

              v_ind := p_opus_clm_list.next(v_ind);

          end loop;

      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         alz_web_process_utils.process_result (0,
                                               9,
                                               -1,
                                               'INVALID_DATA',
                                               SQLERRM,
                                               SQLERRM,
                                               NULL,
                                               NULL,
                                               'ALZ_CLM_DMS_UTILS  ',
                                               NULL,
                                               p_process_results);
   END;




PROCEDURE GET_OPUS_CLM_LIST (
      p_criteria_type     IN     VARCHAR2,
      p_year              IN     NUMBER,
      p_branch            IN     VARCHAR2,
      p_num               IN     VARCHAR2,
      p_name              IN     VARCHAR2,
      p_surname           IN     VARCHAR2,
      p_person_type       IN     VARCHAR2,                  -- i tüzel ,p özel
      p_invoice_no        IN     VARCHAR2,
      p_rim_no            IN     VARCHAR2,
      p_reference_code    IN     VARCHAR2,                          /* acente*/
      p_old_pol           IN     VARCHAR2,
      p_policy_no         IN     VARCHAR2,
      p_plate_no          IN     VARCHAR2,
      p_k_dosya           IN     VARCHAR2,
      p_tax               IN     VARCHAR2,
      p_tck               IN     VARCHAR2,  -- 'T'
      p_tramer_ihbar_no   IN     VARCHAR2,
      p_mondial_no        IN     VARCHAR2,
      p_sozlesme_no       IN     VARCHAR2,
      p_OPUS_CLM_LIST        OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS   IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   --   p_platform  IN VARCHAR2  default 'DMS'
   )
   IS


   BEGIN

    GET_OPUS_CLM_LIST (
      p_criteria_type   ,
      p_year            ,
      p_branch          ,
      p_num             ,
      p_name            ,
      p_surname         ,
      p_person_type     ,                  -- i tüzel ,p özel
      p_invoice_no      ,
      p_rim_no          ,
      p_reference_code  ,                          /* acente*/
      p_old_pol         ,
      p_policy_no       ,
      p_plate_no        ,
      p_k_dosya         ,
      p_tax             ,
      p_tck             ,  -- 'T'
      p_tramer_ihbar_no ,
      p_mondial_no      ,
      p_sozlesme_no     ,
      p_OPUS_CLM_LIST   ,
      P_PROCESS_RESULTS ,
      'DMS'           );


   EXCEPTION
      WHEN OTHERS
      THEN
         alz_web_process_utils.process_result (0,
                                               9,
                                               -1,
                                               'INVALID_DATA',
                                               SQLERRM,
                                               SQLERRM,
                                               NULL,
                                               NULL,
                                               'ALZ_CLM_DMS_UTILS  ',
                                               NULL,
                                               p_process_results);
   END;




PROCEDURE insert_com_docs_index (
   p_clm_com_docs_index_tab   IN OUT clm_com_docs_index_tab,
   p_process_results          IN OUT customer.process_result_table
)
IS
   v_communication_no   NUMBER;
   v_count   NUMBER;
   v_order_no NUMBER;
   v_other_doc_name VARCHAR2(200);
   v_is_insert NUMBER;
   --v_first_communication_no number := 0;
   --v_new_communication_no  number := 0;
   v_activepassive number(1);
   v_doc_count   NUMBER;
   repParams            ALZ_CLM_DMS_MAIN_UTILS.ALZ_CLM_DMS_REPORT_PARAM_ARRAY;
   vRepName VARCHAR2(10);
   v_claim_id number;
   v_sf_no number;
   v_claim_flg number := 0;
   v_sf_no_flg number := 0;
   v_java_param varchar(300);
   v_indexoftire NUMBER;
   v_ext_ref    VARCHAR2 (60);

 cursor c_current_comm_record(p_archive_no number,p_communication_no number,p_ext_reference varchar2,p_document_code varchar2,p_order_no number) is
 select index_date,
        deliver_date,
      doc_sender ,
      department_code,
      is_orginal,
      invoice_date,
      invoice_no,
      invoice_amount,
      swf_code,
      expert_barkod,
      expert_is_orginal,
      explanation,
      comm_user_name ,
      activepassive
from alz_clm_com_docs_index_tbl
 WHERE   archive_no             = p_archive_no
       AND communication_no     = p_communication_no
       AND ext_reference        = p_ext_reference
       AND document_code        = p_document_code
       AND order_no             = p_order_no
       AND NVL(is_scanned, 0)   = 0;

 crec  c_current_comm_record%rowtype;

BEGIN

   BEGIN
      IF p_clm_com_docs_index_tab.COUNT > 0
      THEN

         FOR i IN 1 .. p_clm_com_docs_index_tab.COUNT
         LOOP

            v_is_insert := 1; -- insert
            alz_clm_dms_utils.CHANGE_DOC_STATUS_BY_EXT_REF (v_is_insert, NVL (p_clm_com_docs_index_tab (i).ext_reference, 'X'), p_clm_com_docs_index_tab (i).comm_user_name, p_clm_com_docs_index_tab (i).doc_name);

            -- güncelleme durumu kaldirildi ,  hep yeni giris olacagi kabul edildi.
         /*   IF NVL (p_clm_com_docs_index_tab (i).communication_no, 0) > 0
            THEN
               v_is_insert := 0; -- update
               v_communication_no := p_clm_com_docs_index_tab (i).communication_no; -- zaten hep ayni numarayi aliyor olmali
            END IF;*/
         END LOOP;


      END IF;

      v_communication_no := 0 ;


      IF p_clm_com_docs_index_tab.COUNT > 0
      THEN

         FOR i IN 1 .. p_clm_com_docs_index_tab.COUNT
         LOOP

            IF NVL(p_clm_com_docs_index_tab (i).document_code, '99999') = '99999'
            THEN -- Evrak ekle -> Diger ile eklenen evrak isimlerini cekmek icin
                v_other_doc_name := p_clm_com_docs_index_tab (i).doc_name;
            END IF;

            BEGIN
               v_count := p_process_results.COUNT;
            EXCEPTION
               WHEN OTHERS
               THEN
                  v_count := 0;
            END;

            IF v_count = 0
            THEN
                IF NVL (p_clm_com_docs_index_tab (i).communication_no, 0) = 0
                THEN

                  --  if p_clm_com_docs_index_tab (i).expert_barkod is null and
                    if   NVL (p_clm_com_docs_index_tab (i).archive_no, 0) <> 0  and NVL(p_clm_com_docs_index_tab (i).ext_reference, 'X') = 'X' then
                    -- dosyasiz evraklarda ayni comm no da ayni evrak kodu ile girilmis baska bir evrak var mi, var is yeni muhaberta numarasi alinacak.

                        BEGIN

                           v_doc_count := 0;

                           SELECT COUNT (*)
                             INTO v_doc_count
                             FROM alz_clm_com_docs_index_tbl a
                            WHERE a.archive_no = p_clm_com_docs_index_tab (i).archive_no
                              AND a.communication_no = v_communication_no
                              AND LTRIM (a.document_code, '0') = LTRIM (NVL(p_clm_com_docs_index_tab (i).document_code, '99999'), '0')
                              AND a.ext_reference = NVL (p_clm_com_docs_index_tab (i).ext_reference, 'X');

                        EXCEPTION
                           WHEN OTHERS
                           THEN
                            v_doc_count := 0;
                        END;


                        if  v_doc_count >0 or v_communication_no = 0 then  -- bu evraktan bu comm da var ise
                            alz_clm_dms_utils.get_communication_no (v_communication_no );
                        end if;

                    else -- dosyali evrak durumu her evrak için ayri numara alinacak

                        alz_clm_dms_utils.get_communication_no (v_communication_no);

                    end if;

                    v_order_no := alz_clm_dms_utils.get_max_order (
                                                    v_communication_no, --p_clm_com_docs_index_tab (i).communication_no,
                                                    p_clm_com_docs_index_tab (i).archive_no,
                                                    NVL(p_clm_com_docs_index_tab (i).ext_reference, 'X'),
                                                    NVL(p_clm_com_docs_index_tab (i).document_code, '99999')
                                                );

                    v_order_no := NVL(v_order_no, 0)+1;

                    v_activepassive := 1;
                   -- otoanaliz evraklari hariç digerleri ilk geliste aktif oluyor.
                    if p_clm_com_docs_index_tab (i).expert_barkod is not null then
                        v_activepassive := p_clm_com_docs_index_tab (i).selected;
                    end if;

                    IF NVL (p_clm_com_docs_index_tab (i).archive_no, 0) <> 0
                      THEN

                          -- otoanalizden gelenler hariç yeni giris
                        if p_clm_com_docs_index_tab (i).expert_barkod is null then

                            INSERT INTO alz_clm_com_docs_index_tbl (
                                                                    archive_no,
                                                                    communication_no,
                                                                    ext_reference,
                                                                    document_code,
                                                                    order_no,
                                                                    index_date,
                                                                    deliver_date,
                                                                    doc_sender,
                                                                    department_code,
                                                                    is_orginal,
                                                                    invoice_date,
                                                                    invoice_no,
                                                                    invoice_amount,
                                                                    swf_code,
                                                                    expert_barkod,
                                                                    expert_is_orginal,
                                                                    explanation,
                                                                    comm_user_name,
                                                                    other_name,
                                                                    is_scanned,
                                                                    activepassive,
                                                                    transfer_doc_id
                                    )
                            VALUES    (
                                        p_clm_com_docs_index_tab (i).archive_no,
                                        v_communication_no,
                                        NVL (p_clm_com_docs_index_tab (i).ext_reference, 'X'),
                                        NVL(p_clm_com_docs_index_tab (i).document_code, '99999'),
                                        v_order_no,
                                        p_clm_com_docs_index_tab (i).index_date,
                                        p_clm_com_docs_index_tab (i).deliver_date,
                                        p_clm_com_docs_index_tab (i).doc_sender,
                                        p_clm_com_docs_index_tab (i).department_code,
                                        p_clm_com_docs_index_tab (i).is_orginal,
                                        p_clm_com_docs_index_tab (i).invoice_date,
                                        p_clm_com_docs_index_tab (i).invoice_no,
                                        p_clm_com_docs_index_tab (i).invoice_amount,
                                        p_clm_com_docs_index_tab (i).swf_code,
                                        p_clm_com_docs_index_tab (i).expert_barkod,
                                        p_clm_com_docs_index_tab (i).expert_is_orginal,
                                        p_clm_com_docs_index_tab (i).explanation,
                                        p_clm_com_docs_index_tab (i).comm_user_name,
                                        v_other_doc_name,
                                        0,
                                        v_activepassive,
                                        p_clm_com_docs_index_tab (i).transfer_doc_id
                                    );

                        -- otoanalizden gelen ise   update olacak
                        else
                           v_indexoftire :=  INSTR(p_clm_com_docs_index_tab (i).ext_reference,'-');

                             if v_indexoftire > 0 then

                                v_indexoftire := v_indexoftire-1;
                                v_ext_ref := SUBSTR(p_clm_com_docs_index_tab (i).ext_reference,0,v_indexoftire);
                             else
                                v_ext_ref := p_clm_com_docs_index_tab (i).ext_reference;
                             end if;


                            UPDATE   alz_clm_com_docs_index_tbl
                                SET   index_date        = p_clm_com_docs_index_tab (i).index_date,
                                    deliver_date        = p_clm_com_docs_index_tab (i).deliver_date,
                                    doc_sender        = p_clm_com_docs_index_tab (i).doc_sender,
                                    department_code   = p_clm_com_docs_index_tab (i).department_code,
                                    is_orginal        = decode(p_clm_com_docs_index_tab (i).selected,0,null,p_clm_com_docs_index_tab (i).is_orginal),
                                    invoice_date      = p_clm_com_docs_index_tab (i).invoice_date,
                                    invoice_no        = p_clm_com_docs_index_tab (i).invoice_no,
                                    invoice_amount    = p_clm_com_docs_index_tab (i).invoice_amount,
                                    swf_code          = p_clm_com_docs_index_tab (i).swf_code,
                                    expert_is_orginal = p_clm_com_docs_index_tab (i).expert_is_orginal,
                                    explanation       = p_clm_com_docs_index_tab (i).explanation,
                                    comm_user_name    = p_clm_com_docs_index_tab (i).comm_user_name,
                                    --activepassive     = p_clm_com_docs_index_tab (i).selected,
                                    transfer_doc_id   = p_clm_com_docs_index_tab (i).transfer_doc_id,
                                    archive_no        = p_clm_com_docs_index_tab (i).archive_no,
                                    communication_no  = v_communication_no
                            WHERE   ext_reference     = v_ext_ref
                                and   expert_barkod     = p_clm_com_docs_index_tab (i).expert_barkod
                                and   document_code     = NVL(p_clm_com_docs_index_tab (i).document_code, '99999')
                                and   order_no          = p_clm_com_docs_index_tab (i).order_no;

                        end if;
                        commit;

                        p_clm_com_docs_index_tab (i).communication_no := v_communication_no;

                        -- otoanaliz den gelenler muhaberat tarafindan güncelleniyor
                        if p_clm_com_docs_index_tab (i).expert_barkod is not null and nvl(p_clm_com_docs_index_tab (i).transfer_doc_id ,0) <> 0 then


                             update  ALZ_CLM_DMS_PROCESS_DOCS a
                               set a.IS_ORIGINAL = decode(p_clm_com_docs_index_tab (i).selected,0,null,p_clm_com_docs_index_tab (i).is_orginal)
                             where a.IDX = p_clm_com_docs_index_tab (i).transfer_doc_id;

                             --FileNet geçiþinden dolayý bu tabloyuda güncellemeli
                             update alz_clm_docs
                                  set is_original      = decode(p_clm_com_docs_index_tab (i).selected,0,null,p_clm_com_docs_index_tab (i).is_orginal)
                              where object_id       = p_clm_com_docs_index_tab (i).transfer_doc_id
                                 and ext_reference = v_ext_ref;

                             if p_clm_com_docs_index_tab (i).selected = 0 then

                                 update  ALZ_CLM_DMS_PROCESS_DOCS a
                                   set a.DOC_NOTES = 'Bu evrak Muhaberat Dept. tarafindan Otanalizden gelmedi olarak isaretlenmistir. - '||sysdate ||' //'||a.DOC_NOTES
                                 where a.IDX = p_clm_com_docs_index_tab (i).transfer_doc_id;

                                 --FileNet için eklendi.
                                 update alz_clm_docs
                                      set doc_notes = 'Bu evrak Muhaberat Dept. tarafindan Otanalizden gelmedi olarak isaretlenmistir. - '||sysdate ||' //'|| DOC_NOTES
                                  where object_id       = p_clm_com_docs_index_tab (i).transfer_doc_id
                                     and ext_reference = v_ext_ref;

                             end if;


                            -- fatura bilgisi varsa doc_notes bolumu guncelleneck
                            if  ( nvl(p_clm_com_docs_index_tab (i).invoice_amount, 0) <> 0 ) then

                                update  ALZ_CLM_DMS_PROCESS_DOCS a
                                   set a.DOC_NOTES = SUBSTR (   DECODE (p_clm_com_docs_index_tab (i).INVOICE_DATE,
                                                                           NULL, NULL,
                                                                           'FATURA TARÝHÝ:'
                                                                          )
                                                                || p_clm_com_docs_index_tab (i).INVOICE_DATE
                                                                || ' '
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           'FATURA TUTARI: '
                                                                          )
                                                                || DECODE (ROUND(p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,2),
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           ROUND(p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,2)
                                                                          )
                                                                || ' '
                                                                || p_clm_com_docs_index_tab (i).SWF_CODE
                                                                || ' '
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_NO,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           'FATURA NO: '
                                                                          )
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_NO,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           p_clm_com_docs_index_tab (i).INVOICE_NO
                                                                          ),
                                                                1,
                                                                2500
                                                               ) ||' - '||sysdate||' //'||a.DOC_NOTES
                                 where a.IDX = p_clm_com_docs_index_tab (i).transfer_doc_id;

                                --FileNet için eklendi
                                update alz_clm_docs
                                     set DOC_NOTES = SUBSTR (   DECODE (p_clm_com_docs_index_tab (i).INVOICE_DATE,
                                                                           NULL, NULL,
                                                                           'FATURA TARÝHÝ:'
                                                                          )
                                                                || p_clm_com_docs_index_tab (i).INVOICE_DATE
                                                                || ' '
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           'FATURA TUTARI: '
                                                                          )
                                                                || DECODE (ROUND(p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,2),
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           ROUND(p_clm_com_docs_index_tab (i).INVOICE_AMOUNT,2)
                                                                          )
                                                                || ' '
                                                                || p_clm_com_docs_index_tab (i).SWF_CODE
                                                                || ' '
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_NO,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           'FATURA NO: '
                                                                          )
                                                                || DECODE (p_clm_com_docs_index_tab (i).INVOICE_NO,
                                                                           NULL, NULL,
                                                                           0, NULL,
                                                                           p_clm_com_docs_index_tab (i).INVOICE_NO
                                                                          ),
                                                                1,
                                                                2500
                                                               ) ||' - '||sysdate||' //'||DOC_NOTES
                                 where object_id = p_clm_com_docs_index_tab (i).transfer_doc_id
                                    and ext_reference = v_ext_ref;

                            end if;

                             -- evargin asil fotokogi bilgisi degisti ise evrak üstinde isaretlenecek.
                             if  ( nvl(p_clm_com_docs_index_tab (i).is_orginal,0) <> nvl(p_clm_com_docs_index_tab (i).expert_is_orginal,0) ) then

                                update  ALZ_CLM_DMS_PROCESS_DOCS a
                                   set a.DOC_NOTES = 'Bu evragin Muhaberat Dept. tarafindan asil-fotokopi bilgisi degistirilmistir. - '||sysdate||' //'||a.DOC_NOTES
                                 where a.IDX = p_clm_com_docs_index_tab (i).transfer_doc_id;

                                --FileNet için eklendi
                                update  alz_clm_docs a
                                   set a.DOC_NOTES = 'Bu evragin Muhaberat Dept. tarafindan asil-fotokopi bilgisi degistirilmistir. - '||sysdate||' //'||a.DOC_NOTES
                                 where a.object_id = p_clm_com_docs_index_tab (i).transfer_doc_id
                                   and ext_reference = v_ext_ref;

                             end if;

                             -- dosya iptal  degilse ve  asil ve fatura ise evrak gönderilecek
                             if --( nvl(p_clm_com_docs_index_tab (i).is_orginal,0) <> nvl(p_clm_com_docs_index_tab (i).expert_is_orginal,0) ) or
                                 nvl(p_clm_com_docs_index_tab (i).is_orginal,0) = 1 and p_clm_com_docs_index_tab (i).invoice_date is not null and
                                 GET_CLM_CANCELLED_STATUS(p_clm_com_docs_index_tab (i).ext_reference) <> 1 --and GET_CLM_CLOSED_STATUS(p_clm_com_docs_index_tab (i).ext_reference) <> 1
                                   then

                                  -- icmal için yapilacak olan rapor buradan insret edilecek .

                                  begin
                                     select  claim_id,sf_no into v_claim_id,v_sf_no
                                      from clm_subfiles a where a.ext_reference= NVL (p_clm_com_docs_index_tab (i).ext_reference, 'X') ;
                                  exception when others then v_claim_id := 0 ; v_sf_no := 0 ;
                                  end;

                                  if v_claim_id <> 0 and (v_claim_flg <> v_claim_id and v_sf_no_flg <> v_sf_no) then

                                        begin
                                            repParams(1).PARAM_NAME:='PARAMFORM';
                                            repParams(1).PARAM_VALUE:='NO';
                                            repParams(2).PARAM_NAME:='BACKGROUND';
                                            repParams(2).PARAM_VALUE:='YES';
                                            repParams(3).PARAM_NAME:='P_ARCHIVE_NO';
                                            repParams(3).PARAM_VALUE:=to_char( p_clm_com_docs_index_tab (i).archive_no);
                                            repParams(4).PARAM_NAME:='P_EXT_REFERENCE';
                                            repParams(4).PARAM_VALUE:= rep_res_chr(rtrim(ltrim(p_clm_com_docs_index_tab (i).ext_reference)),1);--p_clm_com_docs_index_tab (i).ext_reference;

                                            vRepName:='OPUS618'; -->kocrepclm915
                                            begin
                                            CUSTOMER.ALZ_CLM_DMS_MAIN_UTILS.createReportInPWByClaimIdSfNo(v_claim_id,v_sf_no,vRepName,repParams,user);
                                            exception when others then null;
                                            end;
                                        end;

                                        v_claim_flg := v_claim_id;
                                        v_sf_no_flg := v_sf_no;

                                  end if;

                             end if;


                        end if;

                    ELSE
                        ROLLBACK;
                        alz_web_process_utils.process_result (
                            0,
                            9,
                            -1,
                            'INVALID_DATA',
                            'Paket No bos olamaz !',
                            'Paket No bos olamaz !',
                            NULL,
                            NULL,
                            'ALZ_CLM_DMS_UTILS, P_CLM_COM_DOCS_INDEX_TAB(i).ARCHIVE_NO  ',
                            NULL,
                            p_process_results
                        );
                    END IF;
                ELSE

                    UPDATE   alz_clm_com_docs_index_tbl
                     SET   index_date        = p_clm_com_docs_index_tab (i).index_date,
                           deliver_date      = p_clm_com_docs_index_tab (i).deliver_date,
                           doc_sender        = p_clm_com_docs_index_tab (i).doc_sender,
                           department_code   = p_clm_com_docs_index_tab (i).department_code,
                           is_orginal        = p_clm_com_docs_index_tab (i).is_orginal,
                           invoice_date      = p_clm_com_docs_index_tab (i).invoice_date,
                           invoice_no        = p_clm_com_docs_index_tab (i).invoice_no,
                           invoice_amount    = p_clm_com_docs_index_tab (i).invoice_amount,
                           swf_code          = p_clm_com_docs_index_tab (i).swf_code,
                           expert_barkod     = NVL(p_clm_com_docs_index_tab (i).expert_barkod, expert_barkod),
                           expert_is_orginal = p_clm_com_docs_index_tab (i).expert_is_orginal,
                           explanation       = p_clm_com_docs_index_tab (i).explanation,
                           comm_user_name    = p_clm_com_docs_index_tab (i).comm_user_name,
                           activepassive     = p_clm_com_docs_index_tab (i).selected
                          -- transfer_doc_id   = p_clm_com_docs_index_tab (i).transfer_doc_id
                    WHERE   archive_no               = p_clm_com_docs_index_tab (i).archive_no
                           AND communication_no     = p_clm_com_docs_index_tab (i).communication_no
                           AND ext_reference        = p_clm_com_docs_index_tab (i).ext_reference
                           AND document_code        = p_clm_com_docs_index_tab (i).document_code
                           AND order_no             = p_clm_com_docs_index_tab (i).order_no
                           AND NVL(is_scanned, 0)   = 0;
                END IF;
            END IF;
         END LOOP;
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         alz_web_process_utils.process_result (0,
                                               9,
                                               -1,
                                               'INVALID_DATA', --substr(sqlerrm,1,100),--'invalid_data',
                                               SQLERRM,
                                               SQLERRM,
                                               NULL,
                                               NULL,
                                               'ALZ_CLM_DMS_UTILS  ',
                                               NULL,
                                               p_process_results);
   END;
END;
 ---------------------------------------------------------------------------------
    PROCEDURE GET_COM_DOC_INDEX_CLM_LIST (
      p_criteria_type      IN     VARCHAR2,
      p_year               IN     NUMBER, -- * F
      p_branch             IN     VARCHAR2, -- * F
      p_num                IN     VARCHAR2, -- * F
      p_name               IN     VARCHAR2,  -- sigortali 'I'  ettiren'IP' magdur 'MD'
      p_surname            IN     VARCHAR2,  --'sigortali I'    ettiren 'E'  magdur 'M'
    --  p_mdr_name           IN     VARCHAR2, -- ???? 'M'
     -- p_mdr_surname        IN     VARCHAR2,  -- ???? 'M'
      p_person_type        IN     VARCHAR2,                 -- i tüzel ,p özel
      p_tck                IN     VARCHAR2,  -- ???? 'T'
      p_tax                IN     VARCHAR2,  --'V'
      p_policy_no          IN     VARCHAR2,  -- 'A'
      p_old_pol            IN     VARCHAR2,
      p_reference_code     IN     VARCHAR2 ,
      p_Communication_No   IN     NUMBER,    --  *'C'
      p_Doc_Sender         IN     VARCHAR2,  --  *'S'                --Gönderen  Adi
      p_plate_no           IN     VARCHAR2, -- 'N'
      p_mdr_plate_no       IN     VARCHAR2,  -- ???? 'P'
      p_Archive_No         IN     NUMBER,    -- * 'R'
      p_Index_Date_start   IN     DATE,      -- * 'G'
      p_Index_Date_end     IN     DATE,      -- * 'G'
      p_Comm_User_Name     IN     VARCHAR2,  -- *  'U'
      p_Department_Code    IN     VARCHAR2,  -- *  'D'
      p_Invoice_Date_start IN     DATE,
      p_Invoice_Date_end   IN     DATE,
      p_invoice_no         IN     VARCHAR2,
      p_noBranch           IN     NUMBER,   --  1: dosyasiz, 0:dosyali, -1:ikiside
      p_explanation        IN     VARCHAR2,
      p_CLM_LIST           OUT OPUS_CLM_LIST_REC_TAB,
      P_PROCESS_RESULTS    IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
      p_platform  IN VARCHAR2  default 'DMS',
      p_Deliver_Date_start IN DATE,
      p_Deliver_Date_end IN DATE,
      p_document_type    IN VARCHAR2, --evrak tipi HUKUK,HASAR
      p_court_file_info  IN VARCHAR2, --esas no
      p_court_enf_no     IN NUMBER,   --mahkeme no
      p_court_enf_type   IN VARCHAR2,  --mahkeme tipi
      p_barcode_no       IN VARCHAR2,  --barkod no
      p_folder_no        IN VARCHAR2  --klasör no
   )
   IS

   v_OPUS_CLM_LIST          OPUS_CLM_LIST_REC_TAB;




    CURSOR C1
      IS
      select distinct ARCHIVE_No, I.COMMUNICATION_NO, I.EXT_REFERENCE,
                I.INDEX_DATE,I.DOC_SENDER,  decode (I.DEPARTMENT_CODE,'BHID', 'Bireysel Oto Hasar Inceleme Departmani'
                                        ,'HDRD', 'Hasar Destek ve Rücu Departmani'
                                        ,'HAD', 'Hasar Arastirma Departmani'
                                        ,'BEHID', 'Bedeni Hasar Inceleme Departmani'
                                        ,'ODHID', 'Oto-Dýþý Hasar Inceleme Departmani') dept_name,

                   I.EXPLANATION, I.COMM_USER_NAME ,x.DENOUNCE_DATE,I.DELIVER_DATE
                 from   ALZ_CLM_COM_DOCS_INDEX_TBL I, clm_subfiles y, koc_clm_detail x
                 where I.EXT_REFERENCE  = y.EXT_REFERENCE
                    and y.CLAIM_ID = x.CLAIM_ID(+)
                    and y.SF_NO = x.SF_NO(+)
                    and x.DETAIL_NO(+) = 1
                    and (  (
                     --p_criteria_type ='F' and
                     (NVL(p_YEAR, 0) = 0 or I.EXT_REFERENCE=p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM)
                         and
                     --p_criteria_type ='C' and
                      (NVL(p_Communication_No, 0) = 0 or I.COMMUNICATION_NO= p_Communication_No)
                          and
                     --p_criteria_type ='S' and
                     (p_DOC_SENDER is null or I.DOC_SENDER= p_DOC_SENDER)
                          and
                     --p_criteria_type ='R' and
                     (NVL(p_ARCHIVE_NO, 0) = 0 or I.ARCHIVE_NO= p_ARCHIVE_NO)
                          and
                     --p_criteria_type ='G' and
                     (p_Index_Date_start is null or I.Index_Date between p_Index_Date_start and p_Index_Date_end)
                          and
                     (p_Deliver_Date_start is null or I.DELIVER_DATE between p_Deliver_Date_start and p_Deliver_Date_end)
                          and
                     --p_criteria_type ='U' and
                     (p_COMM_USER_NAME is null or I.COMM_USER_NAME= p_COMM_USER_NAME)
                          and
                     --p_criteria_type ='D' and
                     (p_DEPARTMENT_CODE is null or I.DEPARTMENT_CODE= p_DEPARTMENT_CODE)
                          and
                     (NVL(p_noBranch, -1) = -1 or ( p_noBranch = 1 and -- dosyasiz evrak
                                                    I.EXT_REFERENCE = 'X' and -- dosyasiz evrak anlaminda sc
                                                    --I.INDEX_DATE BETWEEN p_Index_Date_start and p_Index_Date_end and
                                                    (p_explanation is null or I.EXPLANATION like '%'|| NVL(p_explanation, '') ||'%')
                                                    and I.doc_sender not like '%Faks%'
                                                 )
                                               or ( p_noBranch = 0 and -- dosyali evrak
                                                    I.EXT_REFERENCE <> 'X'-- and
                                                    --I.INDEX_DATE BETWEEN p_Index_Date_start and p_Index_Date_end
                                                 )

                     ) and
                     (p_invoice_no is null or (I.INVOICE_NO = p_invoice_no and
                                                I.INDEX_DATE BETWEEN p_Invoice_Date_start and p_Invoice_Date_end
                                               )
                     )
                 )
                  and (p_criteria_type is null or
                  ( p_criteria_type is not null and
                  I.EXT_REFERENCE in (
                  SELECT
                  EXT_REFERENCE
                  FROM
                    TABLE
                            (CAST
                                (v_OPUS_CLM_LIST --diger kriterlere secilen dosyalar
                                               AS customer.OPUS_CLM_LIST_REC_TAB
                                               )
                                           )
                  )

                 )
                 )
                 )
                 ;

      C1_ROW       C1%ROWTYPE;

      CURSOR C_HUKUK
      IS
      select distinct M.ARCHIVE_NO, M.COMMUNICATION_NO, NVL(M.LAW_FILE_NO, M.UYAP_ID) LAW_FILE_NO,
                M.CREATION_DATE ,M.SEND_FROM,
                M.DESCRIPTION, M.CREATED_BY,M.ARRIVAL_DATE,M.BARCODE_NO,M.FOLDER_NO
                 from alz_law_com_tbl M
                 WHERE (
                     ( NVL(p_YEAR, 0) = 0 or M.LAW_FILE_NO = p_YEAR || ' ' || p_BRANCH || ' ' || p_NUM)

                         and

                      (NVL(p_Communication_No, 0) = 0 or M.COMMUNICATION_NO= p_Communication_No)
                          and

                     (p_DOC_SENDER is null or M.SEND_FROM = p_DOC_SENDER)

                          and

                     (NVL(p_ARCHIVE_NO, 0) = 0 or M.ARCHIVE_NO= p_ARCHIVE_NO)
                          and

                     (p_Index_Date_start is null or TRUNC(M.CREATION_DATE) between p_Index_Date_start and p_Index_Date_end)
                          and
                     (p_Deliver_Date_start is null or TRUNC(M.ARRIVAL_DATE) between p_Deliver_Date_start and p_Deliver_Date_end)
                          and

                     (p_COMM_USER_NAME is null or M.CREATED_BY = p_COMM_USER_NAME)
                          and
                     (p_barcode_no is null or M.BARCODE_NO = p_barcode_no)
                          and
                     (p_folder_no is null or M.FOLDER_NO = p_folder_no)
                          and
                     (p_court_file_info is null or M.COURT_FILE_NO = p_court_file_info)
                          and

                     ((NVL(p_court_enf_no, 0) = 0 and p_court_enf_type is null) or (M.LAW_COURT_NO = p_court_enf_no and M.LAW_COURT_TYPE = p_court_enf_type))
                          and

                     (NVL(p_noBranch, -1) = -1 or ( p_noBranch = 1 and -- dosyasiz evrak
                                                    (M.LAW_FILE_NO = '0' or M.LAW_FILE_NO IS NULL) and
                                                    (p_explanation is null or M.DESCRIPTION like '%'|| NVL(p_explanation, '') ||'%') -- dosyasiz evrak anlaminda sc
                                                 )
                                               or ( p_noBranch = 0 and -- dosyali evrak
                                                    M.LAW_FILE_NO <> '0'-- and

                                                 )

                      )
                      )

                  ;

      C_HUKUK_ROW       C_HUKUK%ROWTYPE;


       CURSOR C2 (cp_Ext_reference in varchar2 )
      IS
      SELECT  distinct A.CLAIM_ID,
                  A.SF_NO,
                  A.EXT_REFERENCE,
                  B.DATE_OF_LOSS,
                  C.DENOUNCE_DATE,
                  DECODE (A.CLM_STATUS,
                          'CLOSED', 'KAPALI',
                          'CANCELLED', 'IPTAL',
                          'AÇIK')
                     STATUS,
                  B.OLDSYS_POLICY_NO,
                  DECODE (B.IS_INDUSTRIAL, 1, 'E', 'B') INDUSTRIAL,
                  A.SF_TYPE,
                  C.DENOUNCER_NAME,
                  B.TERM_START_DATE,
                  B.TERM_END_DATE,
                  B.POLICY_REF
           FROM
             CLM_SUBFILES A, KOC_CLM_SUBFILES_EXT B, KOC_CLM_DETAIL C
          WHERE
                 A.EXT_REFERENCE = cp_Ext_reference
                  AND A.CLAIM_ID = B.CLAIM_ID
                  AND A.SF_NO = B.SF_NO
                  AND B.CLAIM_ID = C.CLAIM_ID
                  AND B.SF_NO = C.SF_NO;


       C2_ROW       C2%ROWTYPE;

      V_status     NUMBER;
      V_messages   VARCHAR2 (100);
      v_AGENT_ID number;

      v_data clob;

      Procedure Log_Data
      Is
         pragma autonomous_transaction;
         v_err clob;
      Begin
         Addrec('v_criteria_type      ' , P_Criteria_Type      , V_Data);
         Addrec('v_year               ' , P_Year               , V_Data);
         Addrec('v_branch             ' , P_Branch             , V_Data);
         Addrec('v_num                ' , P_Num                , V_Data);
         Addrec('v_name               ' , P_Name               , V_Data);
         Addrec('v_surname            ' , P_Surname            , V_Data);
         Addrec('v_person_type        ' , P_Person_Type        , V_Data);
         Addrec('v_tck                ' , P_Tck                , V_Data);
         Addrec('v_tax                ' , P_Tax                , V_Data);
         Addrec('v_policy_no          ' , P_Policy_No          , V_Data);
         Addrec('v_old_pol            ' , P_Old_Pol            , V_Data);
         Addrec('v_reference_code     ' , P_Reference_Code     , V_Data);
         Addrec('v_Communication_No   ' , P_Communication_No   , V_Data);
         Addrec('v_Doc_Sender         ' , P_Doc_Sender         , V_Data);
         Addrec('v_plate_no           ' , P_Plate_No           , V_Data);
         Addrec('v_mdr_plate_no       ' , P_Mdr_Plate_No       , V_Data);
         Addrec('v_Archive_No         ' , P_Archive_No         , V_Data);
         Addrec('v_Index_Date_start   ' , P_Index_Date_Start   , V_Data);
         Addrec('v_Index_Date_end     ' , P_Index_Date_End     , V_Data);
         Addrec('v_Comm_User_Name     ' , P_Comm_User_Name     , V_Data);
         Addrec('v_Department_Code    ' , P_Department_Code    , V_Data);
         Addrec('v_Invoice_Date_start ' , P_Invoice_Date_Start , V_Data);
         Addrec('v_Invoice_Date_end   ' , P_Invoice_Date_End   , V_Data);
         Addrec('v_invoice_no         ' , P_Invoice_No         , V_Data);
         Addrec('v_noBranch           ' , P_Nobranch           , V_Data);
         Addrec('v_explanation        ' , P_Explanation        , V_Data);
         Addrec('v_platform           ' , P_Platform           , V_Data);

         insert into alz_invoice_type_log (data_type, username, log_date, inv_data)
              values ('DMS', USER, SYSDATE, V_DATA);
         commit;
      exception when others then
         v_err := sqlerrm;
         insert into alz_invoice_type_log (data_type, username, log_date, inv_data)
              values ('DMS', 'HATA', SYSDATE, v_err);
              commit;
      End;
   BEGIN
      --log_data;

   IF p_criteria_type = 'V'
         THEN
            FIND_VERGI_NO (p_tax, v_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );

       ELSIF p_criteria_type = 'T'
         THEN
            FIND_TCK_NO (p_tck, v_OPUS_CLM_LIST, P_PROCESS_RESULTS,
                             p_platform );

         ELSIF p_criteria_type = 'F'
         THEN
            FIND_FILE_DETAIL (p_year,
                              p_branch,
                              p_num,
                              v_OPUS_CLM_LIST,
                              P_PROCESS_RESULTS,
                              p_platform);

         ELSIF p_criteria_type = 'N'
         THEN

            FIND_PLATE_DETAIL (p_plate_no,
                               v_OPUS_CLM_LIST,
                               P_PROCESS_RESULTS,
                             p_platform );
         ELSIF p_criteria_type = 'I'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_INS_PERSON_DETAIL (p_name,
                                       p_surname,
                                       v_OPUS_CLM_LIST,
                                       P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_INS_INST_DETAIL (p_name,
                                     v_OPUS_CLM_LIST,
                                     P_PROCESS_RESULTS,
                             p_platform );
            END IF;


         ELSIF p_criteria_type = 'IP'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_PH_PERSON_DETAIL (p_name,
                                      p_surname,
                                      v_OPUS_CLM_LIST,
                                      P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_PH_INST_DETAIL (p_name,
                                    v_OPUS_CLM_LIST,
                                    P_PROCESS_RESULTS,
                             p_platform );
            END IF;
         ELSIF p_criteria_type = 'MD'
         THEN
            IF p_person_type = 'P'
            THEN
               FIND_MDR_PERSON_DETAIL (p_name,
                                       p_surname,
                                       v_OPUS_CLM_LIST,
                                       P_PROCESS_RESULTS,
                             p_platform );
            ELSIF p_person_type = 'I'
            THEN
               FIND_MDR_INST_DETAIL (p_name,
                                     v_OPUS_CLM_LIST,
                                     P_PROCESS_RESULTS,
                             p_platform );
            END IF;
         ELSIF p_criteria_type = 'A'
         THEN
            IF p_POLICY_NO IS NOT NULL
            THEN
               DECLARE
                  V_CONTRACT_ID   NUMBER;
               BEGIN
                  SELECT   CONTRACT_ID
                    INTO   V_CONTRACT_ID
                    FROM   OCP_POLICY_BASES
                   WHERE   POLICY_rEF = p_POLICY_NO AND ROWNUM < 2;

                  FIND_POL_DETAIL (V_CONTRACT_ID,
                                   v_OPUS_CLM_LIST,
                                   P_PROCESS_RESULTS,
                             p_platform );
               END;
            ELSE
               DECLARE
                  V_CONTRACT_ID   NUMBER;
               BEGIN
                  SELECT   CONTRACT_ID
                    INTO   V_CONTRACT_ID
                    FROM   OCP_POLICY_BASES
                   WHERE   POLICY_rEF = p_POLICY_NO AND ROWNUM < 2;

                  BEGIN
                     SELECT   INT_ID
                       INTO   v_AGENT_ID
                       FROM   DMT_AGENTS
                      WHERE   REFERENCE_CODE = p_REFERENCE_CODE;
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        alz_web_process_utils.process_result (
                           0,
                           9,
                           -1,
                           'INVALID_DATA',
                           'Partaj bulunamadi.',
                           'Partaj bulunamadi.',
                           NULL,
                           NULL,
                           'ALZ_CLM_DMS_UTILS , p_REFERENCE_CODE ',
                           NULL,
                           p_process_results
                        );
                  --   p_REFERENCE_CODE :=NULL;
                  END;


                  FIND_FILE_AGENT_DETAIL (V_CONTRACT_ID,
                                          p_reference_code,
                                          v_AGENT_ID,
                                          p_old_pol,
                                          p_policy_no,
                                          v_OPUS_CLM_LIST,
                                          P_PROCESS_RESULTS,
                             p_platform );
               END;
            END IF;

      END IF;

      p_CLM_LIST := OPUS_CLM_LIST_REC_TAB ();

      if  (p_criteria_type is null and  (p_Archive_No <> 0 or  (nvl(p_year, 0) <> 0 and nvl(p_branch, 'X') <> 'X' and nvl(p_num, 'X') <> 'X') or  p_Communication_No <> 0 or p_Doc_Sender is not null or p_invoice_no is not null or p_barcode_no is not null or p_folder_no is not null or p_court_file_info is not null ) )
          or
          (p_court_enf_no <> 0 and p_court_enf_type is not null)
          or
          ((p_Index_Date_start is not null and p_Index_Date_end is not null) and ( p_Department_Code is not null or p_Comm_User_Name is not null or p_noBranch <> 0 ) )
          or
          ((p_Deliver_Date_start is not null and p_Deliver_Date_end is not null) and ( p_Department_Code is not null or p_Comm_User_Name is not null or p_noBranch <> 0 ) )
          then

             IF p_document_type = 'HASAR'
             THEN
              open c1;
              loop
                 fetch c1 into c1_row;

                 exit when c1%notfound;

                p_clm_list.extend;
                p_clm_list (p_clm_list.count) :=
                    customer.opus_clm_list_rec (
                       c1_row.ext_reference,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       c1_row.denounce_date,
                       null,
                       null,
                       c1_row.communication_no,
                       c1_row.archive_no,--null,
                       c1_row.index_date,
                       c1_row.dept_name,
                       c1_row.comm_user_name,
                       c1_row.explanation,
                       c1_row.doc_sender,
                       null,
                       null,
                       null,
                       c1_row.deliver_date,
                       null,
                       null
                    );

              end loop;
              close c1;

              ELSE
              open C_HUKUK;
              loop
                 fetch C_HUKUK into C_HUKUK_ROW;

                 exit when C_HUKUK%notfound;

                p_clm_list.extend;
                p_clm_list (p_clm_list.count) :=
                    customer.opus_clm_list_rec (
                       C_HUKUK_ROW.LAW_FILE_NO,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       null,
                       C_HUKUK_ROW.communication_no,
                       C_HUKUK_ROW.archive_no,
                       C_HUKUK_ROW.creation_date,
                       null,
                       C_HUKUK_ROW.CREATED_BY,
                       C_HUKUK_ROW.DESCRIPTION,
                       C_HUKUK_ROW.send_from,
                       null,
                       null,
                       null,
                       C_HUKUK_ROW.arrival_date,
                       C_HUKUK_ROW.barcode_no,
                       C_HUKUK_ROW.folder_no
                    );

              end loop;
              close C_HUKUK;
              END IF;

      end if;

      IF p_CLM_LIST.COUNT = 0
      THEN
         alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'INVALID_DATA',
            'Girdiginiz arama kriterine ait sistemde açilmis dosya bulunmamaktadir.',
            'Girdiginiz arama kriterine ait sistemde açilmis dosya bulunmamaktadir.',
            NULL,
            NULL,
            'ALZ_CLM_DMS_UTILS , p_OPUS_CLM_LIST.count ',
            NULL,
            p_process_results
         );
      END IF;

   END;




   PROCEDURE GET_COM_DOC_INDEX_INFO (
      p_EXT_REFERENCE                   VARCHAR2,
      p_Communication_No                NUMBER,
      p_Archive_No                      NUMBER,
      p_CLM_COM_DOCS_INDEX_TAB      OUT CLM_COM_DOCS_INDEX_TAB,
      P_PROCESS_RESULTS          IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   )



   IS

    v_CLM_COM_DOCS_INDEX_TAB       CUSTOMER.CLM_COM_DOCS_INDEX_TAB;
    v_EXT_REFERENCE VARCHAR2(100);
   /*
   ARCHIVE_NO  NUMBER, --PAKET NO , ARSIVE NO, ICMAL NO    PK
            COMMUNICATION_NO NUMBER,--MUHABERAT NO  CHAR(10)        PK
            EXT_REFERENCE VARCHAR2(30) , --DOSYA NO   PK
            DOCUMENT_CODE  VARCHAR2(5), --EVRAK KODU                PK
            ORDER_NO NUMBER, --SiRA NO                              PK
            INDEX_DATE DATE ,-- TARIH
            DOC_SENDER VARCHAR2(200), --GÖNDEREN  ADi
            DEPARTMENT_CODE  VARCHAR2(100), -- DEPARTMAN  KODU
            IS_ORGINAL NUMBER(1), -- ASiL 1 /FOTOKOPI 0
            INVOICE_DATE DATE, --FATURA TARIHI
            INVOICE_NO VARCHAR2(100), -- FATURA NO
            INVOICE_AMOUNT NUMBER, --FATURA TUTARi
            SWF_CODE  VARCHAR2(3), --DÖVIZ TIPI
            EXPERT_BARKOD  VARCHAR2(100), --       CLM_SUPPLIERS.SUPP_ID EKSPER BARKODU VARCHAR2
            EXPERT_IS_ORGINAL NUMBER(1), -- EKSPER ASiL/FOTOKOPI BILGISI
            EXPLANATION VARCHAR2(1000),--      AÇiKLAMA
            COMM_USER_NAME VARCHAR2(50),--     MUHABERAT KULLANiCi KODU
            SELECTED  NUMBER(1),
            IS_SCANNED NUMBER(1)
   */
      CURSOR c1
      IS

        SELECT   ARCHIVE_NO,
                 COMMUNICATION_NO,
                 EXT_REFERENCE,
                 DOCUMENT_CODE,
                 ORDER_NO,
                 INDEX_DATE,
                 DOC_SENDER,
                 DEPARTMENT_CODE,
                 IS_ORGINAL,
                 INVOICE_DATE,
                 INVOICE_NO,
                 INVOICE_AMOUNT,
                 SWF_CODE,
                 EXPERT_BARKOD,
                 EXPERT_IS_ORGINAL,
                 EXPLANATION,
                 COMM_USER_NAME,
                 IS_SCANNED,
                 ACTIVEPASSIVE,
                 OTHER_NAME,
                 TRANSFER_DOC_ID,
                 DELIVER_DATE
          FROM   ALZ_CLM_COM_DOCS_INDEX_TBL
         WHERE       ARCHIVE_NO = P_ARCHIVE_NO
                 AND COMMUNICATION_NO = P_communication_no
                 AND EXT_REFERENCE = p_ext_reference
                 AND ACTIVEPASSIVE = 1
        UNION
        -- BURAYI HASAR TÜRÜNE GÖRe DEFAULt EVRAK LISTESINDEN OLUSTUR
        SELECT   ARCHIVE_NO,
                 COMMUNICATION_NO,
                 EXT_REFERENCE,
                 DOCUMENT_CODE,
                 ORDER_NO,
                 INDEX_DATE,
                 DOC_SENDER,
                 DEPARTMENT_CODE,
                 IS_ORGINAL,
                 INVOICE_DATE,
                 INVOICE_NO,
                 INVOICE_AMOUNT,
                 SWF_CODE,
                 EXPERT_BARKOD,
                 EXPERT_IS_ORGINAL,
                 EXPLANATION,
                 COMM_USER_NAME,
                 NULL IS_SCANNED,
                 NULL ACTIVEPASSIVE,
                 NULL OTHER_NAME,
                 NULL TRANSFER_DOC_ID,
                 DELIVER_DATE
          FROM   TABLE(CAST (
                          p_CLM_COM_DOCS_INDEX_TAB    --default evraklar evrak_tablosu
                                                  AS customer.CLM_COM_DOCS_INDEX_TAB
                       ))
         WHERE   DOCUMENT_CODE NOT IN
                       (SELECT   DOCUMENT_CODE
                          FROM   ALZ_CLM_COM_DOCS_INDEX_TBL
                         WHERE       ARCHIVE_NO = P_ARCHIVE_NO
                                 AND COMMUNICATION_NO = P_communication_no
                                 AND EXT_REFERENCE = p_ext_reference
                                 AND ACTIVEPASSIVE = 1);


      C1_ROW   C1%ROWTYPE;
   BEGIN


    --v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);

    DOC_TYPE_LIST_BY_CLM (p_ext_reference,
                                   v_CLM_COM_DOCS_INDEX_TAB  ,
                                   P_PROCESS_RESULTS,
                                   1
                                   );



      p_CLM_COM_DOCS_INDEX_TAB := CLM_COM_DOCS_INDEX_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;

         p_CLM_COM_DOCS_INDEX_TAB.EXTEND;
         p_CLM_COM_DOCS_INDEX_TAB (p_CLM_COM_DOCS_INDEX_TAB.COUNT) :=
            customer.CLM_COM_DOCS_INDEX_REC (C1_ROW.ARCHIVE_NO,
                                             C1_ROW.COMMUNICATION_NO,
                                             C1_ROW.EXT_REFERENCE,
                                             C1_ROW.DOCUMENT_CODE,
                                             C1_ROW.ORDER_NO,
                                             C1_ROW.INDEX_DATE,
                                             C1_ROW.DOC_SENDER,
                                             C1_ROW.DEPARTMENT_CODE,
                                             C1_ROW.IS_ORGINAL,
                                             C1_ROW.INVOICE_DATE,
                                             C1_ROW.INVOICE_NO,
                                             C1_ROW.INVOICE_AMOUNT,
                                             C1_ROW.SWF_CODE,
                                             C1_ROW.EXPERT_BARKOD,
                                             C1_ROW.EXPERT_IS_ORGINAL,
                                             C1_ROW.EXPLANATION,
                                             C1_ROW.COMM_USER_NAME,
                                             C1_ROW.IS_SCANNED,
                                             C1_ROW.ACTIVEPASSIVE,
                                             NVL(C1_ROW.other_name,GET_DOC_NAME (C1_ROW.DOCUMENT_CODE)),
                                             IS_DOC_INVOICE (C1_ROW.DOCUMENT_CODE),
                                             nvl(C1_ROW.TRANSFER_DOC_ID,0),
                                             C1_ROW.DELIVER_DATE
                                             );
      END LOOP;

      CLOSE C1;
   END;

procedure get_max_archive_no(p_archive_no in out number) is
i number;
BEGIN
  i := 1;

   WHILE i = 1
   LOOP

      SELECT alz_dms_indx_archive_seq.NEXTVAL
        INTO p_archive_no
        FROM DUAL;

      --2017 yýlýnda sequence 5 haneyi açtýðý için sequence sýfýrlandý ve unique arþiv numarasý üretebilmesi için yýl eklentisine 10 eklendi
      p_archive_no :=  TO_NUMBER( to_number(TO_CHAR(SYSDATE, 'YY')) + 10 || LPAD (TO_CHAR (p_archive_no), 5, '0') );


      BEGIN
         SELECT 1
           INTO i
           FROM alz_clm_com_docs_index_tbl a
          WHERE a.archive_no = p_archive_no and  rownum <2;

    EXCEPTION
         WHEN OTHERS
         THEN
            i := 0;
      END;
   END LOOP;

END;

 --GELEN EVRAK KAYIT EKRANI SORGULAMA  P_NEW 0 ISE SON KALDIGI PAKET NO, 1 ISE YENI NO VERILECEK
PROCEDURE GET_ARCHIVE_NO_BY_USER (p_user_name    IN     VARCHAR2,
                                     P_NEW          IN     NUMBER,
                                     p_Archive_No    OUT NUMBER)
   IS
      v_Archive_No   NUMBER;
   BEGIN
      IF NVL (p_new, 0) = 0
      THEN
         BEGIN
            SELECT   ARCHIVE_NO
              INTO   p_Archive_No
              FROM   ALZ_COM_DOC_INDX_ARCHV_NO
             WHERE   USERNAME = p_user_name;
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               get_max_archive_no(p_archive_no);

               INSERT INTO ALZ_COM_DOC_INDX_ARCHV_NO (USERNAME, ARCHIVE_NO)
                 VALUES   (p_user_name, p_Archive_No);
         END;
      ELSE
         get_max_archive_no(p_archive_no);

         BEGIN
            SELECT   ARCHIVE_NO
              INTO   v_Archive_No
              FROM   ALZ_COM_DOC_INDX_ARCHV_NO
             WHERE   USERNAME = p_user_name;

            UPDATE   ALZ_COM_DOC_INDX_ARCHV_NO
               SET   ARCHIVE_NO = p_Archive_No
             WHERE   USERNAME = p_user_name;
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               INSERT INTO ALZ_COM_DOC_INDX_ARCHV_NO (USERNAME, ARCHIVE_NO)
                 VALUES   (p_user_name, p_Archive_No);
         END;
      END IF;
   END;


/*
   --GELEN EVRAK KAYIT EKRANI SORGULAMA  P_NEW 0 ISE SON KALDIGI PAKET NO, 1 ISE YENI NO VERILECEK
   PROCEDURE GET_ARCHIVE_NO_BY_USER (p_user_name    IN     VARCHAR2,
                                     P_NEW          IN     NUMBER,
                                     p_Archive_No      OUT NUMBER)
   IS
      v_Archive_No   NUMBER;
   BEGIN
      IF NVL (p_new, 0) = 0
      THEN
         BEGIN
            SELECT   ARCHIVE_NO
              INTO   p_Archive_No
              FROM   ALZ_COM_DOC_INDX_ARCHV_NO
             WHERE   USERNAME = p_user_name;
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               SELECT   ALZ_DMS_INDX_ARCHIVE_SEQ.NEXTVAL
                 INTO   p_Archive_No
                 FROM   DUAL;

               INSERT INTO ALZ_COM_DOC_INDX_ARCHV_NO (USERNAME, ARCHIVE_NO)
                 VALUES   (p_user_name, p_Archive_No);
         END;
      ELSE
         SELECT   ALZ_DMS_INDX_ARCHIVE_SEQ.NEXTVAL INTO p_Archive_No FROM DUAL;



         p_Archive_No:= TO_NUMBER(TO_CHAR(SYSDATE,'YY') ||LPAD(TO_CHAR(p_Archive_No),5,'0'));

         BEGIN
            SELECT   ARCHIVE_NO
              INTO   v_Archive_No
              FROM   ALZ_COM_DOC_INDX_ARCHV_NO
             WHERE   USERNAME = p_user_name;

            UPDATE   ALZ_COM_DOC_INDX_ARCHV_NO
               SET   ARCHIVE_NO = p_Archive_No
             WHERE   USERNAME = p_user_name;
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               INSERT INTO ALZ_COM_DOC_INDX_ARCHV_NO (USERNAME, ARCHIVE_NO)
                 VALUES   (p_user_name, p_Archive_No);
         END;
      END IF;
   END;

*/



 /*PROCEDURE GET_COMMUNICATION_NO (p_Communication_No OUT NUMBER)
   IS
   BEGIN
      SELECT   koc_hlth_indem_comm_no_seq.NEXTVAL
        INTO   p_Communication_No
        FROM   DUAL;
   END;*/

   PROCEDURE GET_COMMUNICATION_NO (p_Communication_No OUT NUMBER)
   IS
   v_comm number;
   BEGIN
        SELECT ALZ_CLM_COM_DOCS_INDEX_SEQ.NEXTVAL
        INTO   V_COMM
        FROM   DUAL;

        p_Communication_No := to_number( to_char(sysdate,'yy')||lpad(v_comm,8,'0'));

   END;




   PROCEDURE GET_COM_DOCS_BY_BARKOD (
      p_barkod                   IN     VARCHAR2,
      p_CLM_COM_DOCS_INDEX_TAB      OUT CLM_COM_DOCS_INDEX_TAB,
      P_PROCESS_RESULTS          IN OUT CUSTOMER.PROCESS_RESULT_TABLE
   )
   IS
      CURSOR c1
      IS
        -- denounce _date index date olarak gönderiliyor, bu tarihe göre milat tarihi kontrol ediliyor .
         SELECT   a.ARCHIVE_NO,
                  a.COMMUNICATION_NO,
                  a.EXT_REFERENCE,
                  a.DOCUMENT_CODE,
                  a.ORDER_NO,
                  trunc(b.DENOUNCE_DATE) INDEX_DATE,
                  A.DELIVER_DATE,
                  a.DOC_SENDER,
                  a.DEPARTMENT_CODE,
                  nvl(IS_ORGINAL,-1) IS_ORGINAL,
                  a.INVOICE_DATE,
                  a.INVOICE_NO,
                  a.INVOICE_AMOUNT,
                  a.SWF_CODE,
                  a.EXPERT_BARKOD,
                  a.EXPERT_IS_ORGINAL,
                  a.EXPLANATION,
                  a.COMM_USER_NAME,
                  a.IS_SCANNED,
                  a.ACTIVEPASSIVE,
                  a.OTHER_NAME,
                  a.TRANSFER_DOC_ID
           FROM   ALZ_CLM_COM_DOCS_INDEX_TBL a, koc_clm_detail b,clm_subfiles c
          WHERE   EXPERT_BARKOD = p_barkod AND ACTIVEPASSIVE = 1
            and a.EXT_REFERENCE = c.EXT_REFERENCE
            and c.CLAIM_ID = b.CLAIM_ID
            and c.SF_NO = b.SF_NO  ;

      C1_ROW   C1%ROWTYPE;
   BEGIN
      p_CLM_COM_DOCS_INDEX_TAB := CLM_COM_DOCS_INDEX_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;

         p_CLM_COM_DOCS_INDEX_TAB.EXTEND;
         p_CLM_COM_DOCS_INDEX_TAB (p_CLM_COM_DOCS_INDEX_TAB.COUNT) :=
            customer.CLM_COM_DOCS_INDEX_REC (C1_ROW.ARCHIVE_NO,
                                             C1_ROW.COMMUNICATION_NO,
                                             C1_ROW.EXT_REFERENCE,
                                             C1_ROW.DOCUMENT_CODE,
                                             C1_ROW.ORDER_NO,
                                             C1_ROW.INDEX_DATE,
                                             C1_ROW.DOC_SENDER,
                                             C1_ROW.DEPARTMENT_CODE,
                                             C1_ROW.IS_ORGINAL,
                                             C1_ROW.INVOICE_DATE,
                                             C1_ROW.INVOICE_NO,
                                             C1_ROW.INVOICE_AMOUNT,
                                             C1_ROW.SWF_CODE,
                                             C1_ROW.EXPERT_BARKOD,
                                             C1_ROW.EXPERT_IS_ORGINAL,
                                             C1_ROW.EXPLANATION,
                                             C1_ROW.COMM_USER_NAME,
                                             C1_ROW.IS_SCANNED,
                                             C1_ROW.ACTIVEPASSIVE,
                                             NVL(C1_ROW.OTHER_NAME,GET_DOC_NAME (C1_ROW.DOCUMENT_CODE)),
                                             IS_DOC_INVOICE(C1_ROW.DOCUMENT_CODE),
                                             nvl(C1_ROW.TRANSFER_DOC_ID,0),
                                             C1_ROW.DELIVER_DATE
                                             );
      END LOOP;

      CLOSE C1;
   END;


   PROCEDURE GET_SENDER_LIST (p_EXT_REFERENCE   IN     VARCHAR2,
                              P_SENDERS            OUT VARCHAR2,
                              p_is_invoice in  number,
                              p_spare_part_suppliers  OUT refcur
                              )
   IS
      v_ext_reference VARCHAR2(100);

      CURSOR SUPP (
         p_CLAIM_ID                 NUMBER,
         p_SF_NO                    NUMBER
      )
      IS
         SELECT   PART_ID, S.supp_id, E.supplier_name
           FROM   CLM_SUPPLIERS S, CLM_ASSIGNMENTS A, koc_clm_suppliers_ext E
          WHERE       A.CLAIM_ID = p_CLAIM_ID
                  AND A.SF_NO = p_SF_NO
                  AND A.end_date IS NULL
                  AND A.SUPP_ID = S.SUPP_ID
                  AND A.supp_id = E.supp_id
                  AND S.SUPP_TYPE in ('EKSP', 'ASER');

      --and alz_eksist_utils.anlasmali_eksper_mi(:boiler.userid, S.supp_id, null) = 1;

      V_PART_ID           NUMBER;

      CURSOR SUPPNAME
      IS
         SELECT   DECODE (A.PARTNER_TYPE,
                          'I', A.INSTITUTION_NAME,
                          A.FIRST_NAME || ' ' || A.SURNAME)
                     TANIM
           FROM   cp_partners A
          WHERE   A.PART_ID = V_PART_ID;

      v_eksper_part_id    cp_partners.part_id%TYPE;

      CURSOR contact_cur (
         p_supp_id NUMBER
      )
      IS
         SELECT   SUBSTR (
                     alz_expert_utils.getworkphone (a.part_id, a.SUPP_ID),
                     INSTR (
                        alz_expert_utils.getworkphone (a.part_id, a.SUPP_ID),
                        ';;',
                        INSTR (
                           alz_expert_utils.getworkphone (a.part_id,
                                                          a.SUPP_ID),
                           ';;'
                        )
                        + 2
                     )
                     + 2
                  )
                     explanation
           FROM   clm_suppliers a, alz_exp_branch_exp_rel c
          WHERE       a.SUPP_ID = c.EXPERT_SUPP_ID
                  AND c.VALIDITY_END_DATE IS NULL
                  AND c.EXPERT_SUPP_ID = p_supp_id;


      C1_ROW              SUPPNAME%ROWTYPE;
      V_SUPP_TYPE         CLM_SUPPLIERS.SUPP_TYPE%TYPE;
      V_TANIM             VARCHAR2 (500);
      v_supp_id           CLM_ASSIGNMENTS.supp_id%TYPE;
      v_contact_no        VARCHAR2 (100) := NULL;  -- 'Tel No Belirtilmemis.';
      v_supplier_name     VARCHAR2 (500);
      V_AGENT_ROLE        clm_pol_bases.AGENT_ROLE%TYPE;

      v_CLAIM_ID          NUMBER;
      v_SF_NO             NUMBER;

      v_SUPP_NAME         VARCHAR2 (200);
      v_SUPP_SHORT_NAME   VARCHAR2 (200);

      v_ASP_NAME          VARCHAR2 (200);

      v_rep_name          VARCHAR2 (200);
      v_rep_adres         VARCHAR2 (200);
      v_rep_tel           VARCHAR2 (200);

      v_ins_name          VARCHAR2 (200);
      v_md_name           VARCHAR2 (200);
      v_Agency_CODE       VARCHAR2 (200);
      v_Agency_name       VARCHAR2 (200);

      CURSOR c2
      IS
         SELECT   b.explanation EXP,
                  DECODE (a.can_be_recourse, 1, 1, 3, 3, 0) rucu,
                  NVL (a.can_be_recourse, 2) rucu1,
                  a.recourse_rate_8 r_8,
                  a.recourse_rate_100 r_100,
                  DECODE (a.has_invoice, 1, 1, 0) fatura,
                  a.explanation,
                  a.in_out kapsam,
                  a.reins_ref_no reas_ref_no,
                  invoice_date,
                  private_code,
                  private_code_explanation,
                  country_code,
                  claim_occured_in_abroad,
                  tramer_claim_status,
                  tramer_recourse_rate,
                  a.claim_handler,
                  a.backup_handler,
                  a.second_backup_handler,
                  a.clam_group,
                  a.is_not_veh_on_sevice,
                  a.veh_place_name,
                  a.veh_place_add_id,
                  a.veh_place_phone,
                  NVL (a.is_resonay, 0) is_resonay
           FROM   koc_dmt_region_code_ref b, koc_clm_detail a
          WHERE       a.claim_id = v_claim_id
                  AND a.sf_no = v_sf_no
                  AND a.user_region_code = b.region_code;

      c2_row              c2%ROWTYPE;

          v_result   refcur;
   -----------------------
   BEGIN

      v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);
      SELECT   G.REFERENCE_CODE,
               E.TITLE,
               A.CLAIM_ID,
               A.SF_NO
        INTO   v_Agency_CODE,
               v_Agency_name,
               v_CLAIM_ID,
               v_SF_NO
        FROM   CLM_SUBFILES A,
               KOC_CLM_SUBFILES_EXT B,
               KOC_CLM_DETAIL C,
               CLM_POL_BASES F,
               DMT_AGENTS G,
               KOC_DMT_AGENTS_EXT E
       WHERE       A.EXT_REFERENCE = v_ext_reference
               AND A.CLAIM_ID = B.CLAIM_ID
               AND A.SF_NO = B.SF_NO
               AND B.CLAIM_ID = F.CLAIM_ID
               AND B.CLAIM_ID = C.CLAIM_ID
               AND B.SF_NO = C.SF_NO
               AND F.AGENT_ROLE = G.INT_ID
               AND G.INT_ID = E.INT_ID;


if  nvl(p_is_invoice,0)=0 then



      OPEN SUPP (v_CLAIM_ID, v_SF_NO);

      LOOP
         FETCH SUPP
         INTO   V_PART_ID, v_supp_id, v_supplier_name;

         EXIT WHEN SUPP%NOTFOUND;

         OPEN SUPPNAME;

         FETCH SUPPNAME INTO C1_ROW;

         IF SUPPNAME%FOUND
         THEN
            V_TANIM := C1_ROW.TANIM;
         ELSE
            --OPU001.MSG (160370,'E','KOC_CP_V_PARTNERS');
            V_TANIM := NULL;
            v_SUPP_NAME := NULL;
            v_SUPP_SHORT_NAME := NULL;
         END IF;

         CLOSE SUPPNAME;


         v_SUPP_NAME :=
               v_supp_id
            || ' - '
            || V_TANIM
            || '-'
            || alz_expert_utils.get_comm_info (v_supp_id, '0010');
         v_eksper_part_id := v_part_id;
         v_SUPP_SHORT_NAME := v_supplier_name;
      END LOOP;

      CLOSE SUPP;

      IF v_eksper_part_id IS NOT NULL
      THEN
         FOR contact_rec IN contact_cur (v_eksper_part_id)
         LOOP
            --    FOR contact_rec IN contact_cur LOOP
            IF contact_rec.explanation IS NOT NULL
            THEN
               v_contact_no := v_contact_no || ' ' || contact_rec.explanation;
            ELSE
               v_contact_no := v_contact_no;
            END IF;
         END LOOP;

         --musti@kora CTP
         --:MOTOR_DETAIL.SUPP_NAME :=    :MOTOR_DETAIL.SUPP_NAME||' - '||v_contact_no;
         v_SUPP_NAME := v_SUPP_NAME || ' - ' || v_contact_no;
      ELSE
         v_SUPP_NAME := NULL;
         v_SUPP_SHORT_NAME := NULL;
      END IF;



      BEGIN
         SELECT   DISTINCT B.NAME
           INTO   v_ASP_NAME
           FROM   CLM_SUPPLIERS A, CP_V_PARTNERS B, CLM_ASSIGNMENTS C
          WHERE       C.CLAIM_ID = v_CLAIM_ID
                  AND C.SF_NO = v_SF_NO
                  AND C.SUPP_ID = A.SUPP_ID
                  AND A.SUPP_TYPE = 'ASER'
                  AND A.PART_ID = B.PART_ID
                  AND C.ASS_ID =
                        (SELECT   MAX (A1.ASS_ID)
                           FROM   CLM_ASSIGNMENTS A1, CLM_SUPPLIERS C1
                          WHERE   A1.CLAIM_ID = C.CLAIM_ID
                                  AND A1.SF_NO = C.SF_NO
                                  AND C1.SUPP_TYPE IN
                                           ('ASER',
                                            'TMR',
                                            'ACS',
                                            'ART',
                                            'TAS')
                                  AND C1.SUPP_ID = A1.SUPP_ID);
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;


      -----------------

      BEGIN
         SELECT   repair_shop_name, repair_shop_address, repair_shop_tel
           INTO   v_rep_name, v_rep_adres, v_rep_tel
           FROM   koc_clm_veh_info
          WHERE   claim_id = v_claim_id AND sf_no = v_sf_no;
      EXCEPTION
         WHEN OTHERS
         THEN
            v_rep_name := NULL;
            v_rep_adres := NULL;
            v_rep_tel := NULL;
      END;

      OPEN c2;

      FETCH c2 INTO c2_row;

      IF c2%FOUND
      THEN
         IF NVL (c2_row.is_not_veh_on_sevice, 0) = 1 AND v_rep_name IS NULL
         THEN
            v_rep_name := c2_row.veh_place_name;
            v_rep_adres :=
               Koc_Address_Utils2.address (c2_row.veh_place_add_id);
            v_rep_tel := c2_row.veh_place_phone;
         END IF;
      END IF;

      CLOSE c2;

      SELECT   KOC_CLM_UTILS.INS_NAME_BUL (v_CLAIM_ID)
        INTO   v_ins_name
        FROM   DUAL;

      SELECT   KOC_CLM_UTILS.MDR_NAME_BUL (v_CLAIM_ID, v_SF_NO)
        INTO   v_md_name
        FROM   DUAL;



      P_SENDERS := '';

      IF v_SUPP_NAME IS NOT NULL
      THEN
         P_SENDERS := 'Eksper' || ':' || v_SUPP_NAME;
      END IF;

      IF v_ins_name IS NOT NULL
      THEN
         IF LENGTH (P_SENDERS) > 0
         THEN
            P_SENDERS := p_senders || '*_*' || 'Sigortali' || ':' || v_ins_name;
         ELSE
            P_SENDERS := 'Sigortali' || ':' || v_ins_name;
         END IF;
      END IF;

      IF v_md_name IS NOT NULL
      THEN
         IF LENGTH (P_SENDERS) > 0
         THEN
            P_SENDERS := p_senders || '*_*' || 'Magdur' || ':' || v_md_name;
         ELSE
            P_SENDERS := 'Magdur' || ':' || v_md_name;
         END IF;
      END IF;


      IF v_asp_name IS NOT NULL
      THEN
         IF LENGTH (P_SENDERS) > 0
         THEN
            P_SENDERS := p_senders || '*_*' || 'Anlasmali Servis' || ':' || v_asp_name;
         ELSE
            P_SENDERS := 'Anlasmali Servis' || ':' || v_asp_name;
         END IF;
      END IF;

      IF v_REP_NAME IS NOT NULL
      THEN
         IF LENGTH (P_SENDERS) > 0
         THEN
            P_SENDERS := p_senders || '*_*' || 'Tamirhane' || ':' || v_REP_NAME;
         ELSE
            P_SENDERS := 'Tamirhane' || ':' || v_REP_NAME;
         END IF;
      END IF;

      IF v_Agency_name IS NOT NULL
      THEN
         IF LENGTH (P_SENDERS) > 0
         THEN
            P_SENDERS := p_senders || '*_*' || 'Acente' || ':' || v_Agency_name;
         ELSE
            P_SENDERS := 'Acente' || ':' || v_Agency_name;
         END IF;
      END IF;

     else



         OPEN v_result FOR
         select A.SUPP_ID,
            (SELECT DECODE(b.PARTNER_TYPE,'I',b.NAME,'P',b.FIRST_NAME||' '||b.NAME) ROL_NAME
                    FROM CP_V_PARTNERS b
                     WHERE   b.PART_ID=a.PART_ID) SUPP_NAME, A.PART_ID
              from CLM_SUPPLIERS a
              where a.SUPP_TYPE='YPT'
                and (a.EXP_DATE is null or exp_date > sysdate );

         /*
         select A.SUPP_ID,
            (SELECT DECODE(b.PARTNER_TYPE,'I',b.NAME,'P',b.FIRST_NAME||' '||b.NAME) ROL_NAME
                    FROM CP_V_PARTNERS b
                     WHERE   b.PART_ID=a.PART_ID) SUPP_NAME
              from CLM_SUPPLIERS a
              where a.SUPP_TYPE='YPT'
                and (a.EXP_DATE is null or exp_date > sysdate );*/

      p_spare_part_suppliers := v_result;


     end if;



   END;


   PROCEDURE GET_DEPARTMENTS_LIST (P_DEPATMENT_LIST OUT refcur)
   IS
      v_result   refcur;
   BEGIN
      OPEN v_result FOR
         SELECT   'BHID', 'Bireysel Oto Hasar Inceleme Departmani' FROM DUAL
         UNION
         SELECT   'HDRD', 'Hasar Destek ve Rücu Departmani' FROM DUAL
         UNION
         SELECT   'HAD', 'Hasar Arastirma Departmani' FROM DUAL
         UNION
         SELECT   'ODHID', 'Oto-Dýþý Hasar Inceleme Departmani' FROM DUAL
         UNION
         SELECT   'BEHID', 'Bedeni Hasar Inceleme Departmani' FROM DUAL;

      P_DEPATMENT_LIST := v_result;
   END;

 -- bu liste dosya türüne göre hepsi içerisinden kistlanacak olabilir mi ? ha ??
 /*
  PROCEDURE DOC_TYPE_LIST_BY_CLM_old (p_EXT_REFERENCE   IN     VARCHAR2,
                                   P_CLM_COM_DOCS_INDEX_TAB   IN OUT CLM_COM_DOCS_INDEX_TAB,
                                   P_PROCESS_RESULTS          IN OUT CUSTOMER.PROCESS_RESULT_TABLE,
                                   P_DEFAULT_DOC    IN NUMBER  -- p_default_doc =0 ile iliskili tüm evrak listesi gelecek,
                                                                -- p_default_doc =1 ile iliskili tüm evrak listesi üzerinden default olanlar gelecek
                                   )
   IS

     v_claim_id          NUMBER (10);
     v_sf_no             NUMBER (4);
     v_sf_type           varchar2 (10);

CURSOR C1 (p_claim_id  NUMBER,p_sf_no  NUMBER, p_sf_type varchar2 )IS
SELECT decode(l.cause1_code,'930','930',DECODE(p_sf_type,'T',L.CAUSE1_CODE,'TS',L.CAUSE1_CODE,'999')) cause1_code,
CAUSE1_CODE CAUSE
FROM  KOC_CLM_SUBFILE_CAUSE1_REL L
where  CLAIM_ID=p_CLAIM_ID
AND  SF_NO   =p_SF_NO
and l.cancel_date is null;
--and l.default_doc=P_DEFAULT_DOC;-- alanina bilgi girisi ve eksik olan evraklarinda tanimi yapilmali  aç ha ??

C1_ROW C1%ROWTYPE;
V_IP_NO NUMBER;

------insert doc

PROCEDURE insert_DOC ( p_EXT_REFERENCE   IN     VARCHAR2,
                        p_claim_id        IN clm_bases.CLAIM_ID%TYPE,
                                     p_sf_no           IN clm_subfiles.SF_NO%TYPE,
                                     p_sf_type         IN clm_subfiles.SF_TYPE%TYPE,
                   p_cause         IN koc_cc_loss_cause1.cause1_code%TYPE,
                   V_CAUSE1_CODE         IN koc_cc_loss_cause1.cause1_code%TYPE,
--                                     p_username        IN koc_clm_doc_info.USERNAME%TYPE,
                                     p_date            IN DATE,
                                     P_CLM_COM_DOCS_INDEX_TAB   IN OUT CLM_COM_DOCS_INDEX_TAB,
                                   P_PROCESS_RESULTS          IN OUT CUSTOMER.PROCESS_RESULT_TABLE
                                     ) IS

CURSOR ip IS
      SELECT MAX(ip_no)
      FROM clm_interested_parties
      WHERE claim_id=p_claim_id;


CURSOR doc_order IS
        SELECT MAX(NVL(doc_order_no,0))
        FROM koc_clm_doc_info
        WHERE claim_id=p_claim_id
        and sf_no= p_sf_no
        AND DOC_CODE <>999;

V_KONTROL NUMBER;

CURSOR DOC IS
    SELECT doc_code
    FROM KOC_CLM_SF_COV_DOC_REL
    WHERE sf_type=p_sf_type AND
          cause1_code= p_cause AND
          validity_end_date IS NULL;




CURSOR DOC2 IS
select DOC_CODE  from koc_clm_reason_doc_ref f , koc_clm_doc_reason N
where f.REASON_CODE = n.REASON_CODE and f.REASON_DETAIL_CODE=n.REASON_DETAIL_CODE
and N.CLAIM_ID=p_CLAIM_ID AND N.Sf_NO= P_SF_NO
and n.VALIDITY_END_DATE is null
UNION
select DOC_CODE  from koc_clm_reason_doc_ref f
where
( f.HAS_LAW_FILE = 1 and
  f.HAS_LAW_FILE =(SELECT HAS_LAW_FILE FROM KOC_CLM_DETAIL O WHERE  O.CLAIM_ID=P_CLAIM_ID
  AND O.SF_NO= P_SF_NO)
)
or
( f.CAN_BE_RECOURSE=1 and
  f.CAN_BE_RECOURSE=(SELECT CAN_BE_RECOURSE FROM KOC_CLM_DETAIL O WHERE O.CLAIM_ID=P_CLAIM_ID
  AND O.SF_NO= P_SF_NO)
)
union
SELECT DOC_CODE FROM koc_clm_reason_doc_ref F WHERE f.REASON_CODE = 99 and f.REASON_DETAIL_CODE=99
and f.HAS_LAW_FILE = 0 and f.CAN_BE_RECOURSE=0;
-------------------------------

v_doc_code      koc_clm_sf_cov_doc_rel.DOC_CODE%TYPE;
v_doc_order_no  koc_clm_doc_info.DOC_ORDER_NO%TYPE;
V_IP_NO NUMBER;

v_sf_type varchar2(5);
v_PERT_I varchar2(10);
BEGIN



   v_sf_type:= p_sf_type;
  V_KONTROL:=0;

         OPEN ip;
         FETCH ip INTO v_ip_no;
         CLOSE ip;
             OPEN doc_order;
             FETCH doc_order INTO v_doc_order_no;
             CLOSE doc_order;

          if v_sf_type in ( 'CS' , 'CH' , 'KS' , 'CF' ) then

            OPEN doc2;
          else
            OPEN doc;
          end if;



      LOOP
          if v_sf_type in ( 'CS' , 'CH' , 'KS' ,'CF') then

              FETCH doc2 INTO v_doc_code;
                EXIT WHEN doc2%NOTFOUND;
          else
                FETCH doc INTO v_doc_code;
                EXIT WHEN doc%NOTFOUND;
              end if;

                      v_doc_order_no:=NVL(v_doc_order_no,0)+1;

                      SELECT DECODE(p_sf_type,'KR',NULL,v_ip_no) INTO V_IP_NO
                      FROM DUAL;


BEGIN
     SELECT CLAIM_ID INTO V_KONTROL  FROM KOC_CLM_DOC_INFO
   where  CLAIM_ID=p_claim_id
   AND   SF_NO   =p_sf_no
   AND     DOC_CODE = v_doc_code
   and  (nvl(IS_VALID,0) = 1 OR NVL(IS_RECEIVED,0) = 1);
EXCEPTION WHEN NO_DATA_FOUND THEN  V_KONTROL:=0;
END;

               IF NVL(  V_KONTROL,0) = 0 THEN

            p_CLM_COM_DOCS_INDEX_TAB.extend;

           p_CLM_COM_DOCS_INDEX_TAB (p_CLM_COM_DOCS_INDEX_TAB.COUNT) :=

            customer.CLM_COM_DOCS_INDEX_REC (null,--C1_ROW.ARCHIVE_NO,
                                             null,--C1_ROW.COMMUNICATION_NO,
                                             p_EXT_REFERENCE,
                                             v_doc_code,
                                             v_doc_order_no,--C1_ROW.ORDER_NO,
                                             null,--C1_ROW.INDEX_DATE,
                                             null,--C1_ROW.DOC_SENDER,
                                             null,--C1_ROW.DEPARTMENT_CODE,
                                             null,--C1_ROW.IS_ORGINAL,
                                               null,--C1_ROW.INVOICE_DATE,
                                               null,--C1_ROW.INVOICE_NO,
                                               null,--C1_ROW.INVOICE_AMOUNT,
                                               null,--C1_ROW.SWF_CODE,
                                               null,--C1_ROW.EXPERT_BARKOD,
                                               null,--C1_ROW.EXPERT_IS_ORGINAL,
                                               null,--C1_ROW.EXPLANATION,
                                               null,--C1_ROW.COMM_USER_NAME,
                                               null,--C1_ROW.IS_SCANNED,
                                               null,--C1_ROW.ACTIVEPASSIVE
                                               GET_DOC_NAME (v_doc_code),
                                               IS_DOC_INVOICE(v_doc_code)
                                             );


END IF;


                END LOOP;

--PERT
v_PERT_I:=null;
BEGIN
 SELECT  L.RESULT_CODE INTO v_PERT_I
 FROM KOC_CLM_SUBFILE_RESULT_REL L
 WHERE L.CLAIM_ID=P_CLAIM_ID
 AND    L.SF_NO=p_SF_NO
 AND L.CANCEL_DATE IS NULL;
 EXCEPTION WHEN OTHERS THEN
v_PERT_I:=NULL;
     END;


IF NVL(v_PERT_I,'X') <> 'X' THEN

 p_CLM_COM_DOCS_INDEX_TAB.extend;

           p_CLM_COM_DOCS_INDEX_TAB (p_CLM_COM_DOCS_INDEX_TAB.COUNT) :=

            customer.CLM_COM_DOCS_INDEX_REC (null,--C1_ROW.ARCHIVE_NO,
                                             null,--C1_ROW.COMMUNICATION_NO,
                                             p_EXT_REFERENCE,
                                             '9956',
                                             999,--C1_ROW.ORDER_NO,
                                             null,--C1_ROW.INDEX_DATE,
                                             null,--C1_ROW.DOC_SENDER,
                                             null,--C1_ROW.DEPARTMENT_CODE,
                                             null,--C1_ROW.IS_ORGINAL,
                                               null,--C1_ROW.INVOICE_DATE,
                                               null,--C1_ROW.INVOICE_NO,
                                               null,--C1_ROW.INVOICE_AMOUNT,
                                               null,--C1_ROW.SWF_CODE,
                                               null,--C1_ROW.EXPERT_BARKOD,
                                               null,--C1_ROW.EXPERT_IS_ORGINAL,
                                               null,--C1_ROW.EXPLANATION,
                                               null,--C1_ROW.COMM_USER_NAME,
                                               null,--C1_ROW.IS_SCANNED,
                                               null,--C1_ROW.ACTIVEPASSIVE
                                                GET_DOC_NAME ('9956'),
                                                IS_DOC_INVOICE('9956')
                                             );



END IF;

if v_sf_type in ( 'CS' , 'CH' , 'KS' ,'CF') then
    CLOSE doc2;
else
  CLOSE doc;
end if;






END ;

-----insert doc


   BEGIN


    p_CLM_COM_DOCS_INDEX_TAB := CLM_COM_DOCS_INDEX_TAB ();


         SELECT  b.claim_id,
                  b.sf_no,
                  B.SF_TYPE
           INTO   v_claim_id,
                  v_sf_no,
                  v_sf_type
           FROM   clm_subfiles b
          WHERE   B.ext_reference = p_ext_reference;


     OPEN C1( v_claim_id  ,v_sf_no , v_sf_type);
    FETCH C1 INTO C1_ROW;
    IF C1%FOUND THEN


     INSERT_DOC (p_EXT_REFERENCE ,v_CLAIM_ID,v_sf_no,
      v_SF_TYPE||'R',C1_ROW.CAUSE1_CODE, C1_ROW.CAUSE,
       --p_USERID,
       SYSDATE,
       P_CLM_COM_DOCS_INDEX_TAB   ,
       P_PROCESS_RESULTS
       );

    END IF;
    CLOSE C1;

 -------------------------------------------------------------------------------------------------------------

   --P_DOC_TYPE_LIST:= v_result ;

   END;

-------------------
*/
PROCEDURE doc_type_list (
   p_clm_com_docs_index_tab   IN OUT clm_com_docs_index_tab,
   p_process_results          IN OUT customer.process_result_table
)
IS
   CURSOR c2
   IS
      SELECT   a.doc_code, a.explanation, a.is_invoice, A.default_doc_order, DECODE (NVL(A.default_doc_order, 0), 0, 0, 1) isDefault
        FROM   koc_clm_doc_ref a
       WHERE   a.validity_end_date IS NULL OR a.validity_end_date > SYSDATE;

   c2_row           c2%ROWTYPE;
   v_explanation    koc_clm_doc_ref.explanation%TYPE;
   --v_doc_order_no   NUMBER := 0;
BEGIN
   p_clm_com_docs_index_tab := clm_com_docs_index_tab ();

   ---------------------------------------------------------------------------------------------------------
   OPEN c2;

   LOOP
      FETCH c2 INTO c2_row;

      EXIT WHEN c2%NOTFOUND;

      IF c2%FOUND
      THEN

         p_clm_com_docs_index_tab.EXTEND;
         --v_doc_order_no := NVL (v_doc_order_no, 0) + 1;
         p_clm_com_docs_index_tab (p_clm_com_docs_index_tab.COUNT) :=
            customer.clm_com_docs_index_rec (NULL,              --c1_row.archive_no,
                                             NULL,              --c1_row.communication_no,
                                             NULL,              --ext_reference
                                             c2_row.doc_code,
                                             c2_row.default_doc_order,  --c1_row.order_no,
                                             NULL,              --c1_row.index_date,
                                             NULL,              --c1_row.doc_sender,
                                             NULL,              --c1_row.department_code,
                                             NULL,              --c1_row.is_orginal,
                                             NULL,              --c1_row.invoice_date,
                                             NULL,              --c1_row.invoice_no,
                                             NULL,              --c1_row.invoice_amount,
                                             NULL,              --c1_row.swf_code,
                                             NULL,              --c1_row.expert_barkod,
                                             NULL,              --c1_row.expert_is_orginal,
                                             NULL,              --c1_row.explanation,
                                             NULL,              --c1_row.comm_user_name,
                                             NVL(c2_row.isDefault, 0), --c1_row.activepassive -- isDefault Doc
                                             NULL,              --c1_row.is_scanned,
                                             c2_row.explanation, --get_doc_name (c2_row.doc_code),
                                             NVL(c2_row.is_invoice, 0),   --is_doc_invoice (c2_row.doc_code)
                                             0,
                                             NULL              --c1_row.deliver_date,
                                                              );
      END IF;
   END LOOP;

   CLOSE c2;

   exception when others then raise_application_error(-20100, c2_row.doc_code
                                                  || ' - ' || c2_row.default_doc_order
                                                  || ' - ' || c2_row.explanation
                                                  || ' - ' || NVL(c2_row.is_invoice, 0)
                                                  || ' : ' || sqlerrm
                                                  );
END;

----------------------------------------------------------------------
PROCEDURE doc_type_list_by_clm (
   p_ext_reference            IN       VARCHAR2,
   p_clm_com_docs_index_tab   IN OUT   clm_com_docs_index_tab,
   p_process_results          IN OUT   customer.process_result_table,
   p_default_doc              IN       NUMBER

-- p_default_doc =0 ile iliskili tüm evrak listesi gelecek,
-- p_default_doc =1 ile iliskili tüm evrak listesi üzerinden default olanlar gelecek
)
IS
   v_claim_id       NUMBER (10);
   v_sf_no          NUMBER (4);
   v_sf_type        VARCHAR2 (10);
   v_ext_reference  VARCHAR2 (100);

   CURSOR c2 --(cp_sf_type VARCHAR2)
   IS
           /*SELECT distinct a.DOC_CODE
                 FROM koc_clm_sf_cov_doc_rel a, koc_clm_doc_ref b
                WHERE a.DOC_CODE = b.DOC_CODE
                  and a.sf_type = cp_sf_type
                 and (b.VALIDITY_END_DATE is null or b.VALIDITY_END_DATE > sysdate )
                 and (a.VALIDITY_END_DATE is null or a.VALIDITY_END_DATE > sysdate )
                  AND NVL (a.default_doc, 0) = p_default_doc;*/
             SELECT a.doc_code, a.explanation, a.is_invoice, A.default_doc_order, DECODE (NVL(A.default_doc_order, 0), 0, 0, 1) isDefault
                 FROM koc_clm_doc_ref a
                WHERE
                     (a.VALIDITY_END_DATE is null or a.VALIDITY_END_DATE > sysdate )
                    AND DECODE( NVL (A.DEFAULT_DOC_ORDER, 0), 0, 0, 1) = p_default_doc;

   c2_row           c2%ROWTYPE;
   v_ip_no          NUMBER;
   v_explanation    koc_clm_doc_ref.explanation%TYPE;
   --v_doc_order_no   NUMBER                             := 0;
BEGIN

   v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);
   p_clm_com_docs_index_tab := clm_com_docs_index_tab ();

   /*
    SELECT b.claim_id, b.sf_no, b.sf_type
     INTO v_claim_id, v_sf_no, v_sf_type
     FROM clm_subfiles b
    WHERE b.ext_reference = v_ext_reference;
    */

---------------------------------------------------------------------------------------------------------
   OPEN c2; --(v_sf_type);

   LOOP
      FETCH c2
       INTO c2_row;

      EXIT WHEN c2%NOTFOUND;

      IF c2%FOUND
      THEN

         p_clm_com_docs_index_tab.EXTEND;
         --v_doc_order_no := NVL (v_doc_order_no, 0) + 1;
         p_clm_com_docs_index_tab (p_clm_com_docs_index_tab.COUNT) :=
            customer.clm_com_docs_index_rec
                            (NULL,                        --c1_row.archive_no,
                             NULL,                  --c1_row.communication_no,
                             p_ext_reference,
                             c2_row.doc_code,
                             c2_row.default_doc_order,  --c1_row.order_no,
                             NULL,                        --c1_row.index_date,
                             NULL,                        --c1_row.doc_sender,
                             NULL,                   --c1_row.department_code,
                             NULL,                        --c1_row.is_orginal,
                             NULL,                      --c1_row.invoice_date,
                             NULL,                        --c1_row.invoice_no,
                             NULL,                    --c1_row.invoice_amount,
                             NULL,                          --c1_row.swf_code,
                             NULL,                     --c1_row.expert_barkod,
                             NULL,                 --c1_row.expert_is_orginal,
                             NULL,                       --c1_row.explanation,
                             NULL,                    --c1_row.comm_user_name,
                             1,
                               --c1_row.activepassive -- hepsi aktif görünmeli
                             NULL,                        --c1_row.is_scanned,
                             c2_row.explanation,--get_doc_name (c2_row.doc_code),
                             c2_row.is_invoice,--is_doc_invoice (c2_row.doc_code)
                             0,
                             NULL                        --c1_row.deliver_date,
                            );
      END IF;
   END LOOP;

   CLOSE c2;

   exception when others then raise_application_error(-20100, c2_row.doc_code
                                                  || ' - ' || c2_row.default_doc_order
                                                  || ' - ' || c2_row.explanation
                                                  || ' - ' || NVL(c2_row.is_invoice, 0)
                                                  || ' : ' || sqlerrm
                                                  );

END;
----------------------------------------------------------------------

 /* -- bir üstteki   DOC_TYPE_LIST_BY_CLM isimli prosedüre p_default_doc alani eklenerek iptal edildi.
 -- p_default_doc =0 ile iliskili tüm evrak listesi gelecek
 --DOCUMENT_CODE,IS_INVOICE, ORDER_NO, DOCUMENT_NAME
  -- bu liste dosya türüne göre hepsi olabilir mi ? ha ??
   PROCEDURE DOC_TYPE_LIST (P_DOC_TYPE_LIST OUT refcur)
   IS
  v_result   refcur;

   BEGIN

    OPEN v_result FOR
   select distinct a.DOC_CODE, B.EXPLANATION from KOC_CLM_SF_COV_DOC_REL a,
                         KOC_CLM_DOC_REF b
   where A.DOC_CODE=B.DOC_CODE and
         B.VALIDITY_END_DATE is null and A.VALIDITY_END_DATE is null
         order by A.DOC_CODE;


   P_DOC_TYPE_LIST:= v_result ;




   END;*/

   --DOCUMENT_CODE,IS_INVOICE, ORDER_NO, DOCUMENT_NAME




procedure doc_type_com_list_by_clm (
   p_ext_reference varchar2,
   p_communication_no number,
   p_clm_com_docs_index_tab   in out clm_com_docs_index_tab,
   p_process_results          in out customer.process_result_table

)
is

   v_ext_reference  VARCHAR2(100);

   cursor c1 is

SELECT   a.archive_no,
         a.communication_no,
         a.ext_reference,
         a.document_code,
         a.order_no,
         a.is_orginal,
         a.invoice_amount,
         a.swf_code,
         a.invoice_date,
         a.invoice_no,
         a.activepassive,
         a.other_name,
         A.TRANSFER_DOC_ID
  FROM   alz_clm_com_docs_index_tbl a
 WHERE   a.ext_reference = p_ext_reference
         AND a.communication_no = p_communication_no;

   c1_row           c1%rowtype;
begin

   p_clm_com_docs_index_tab := clm_com_docs_index_tab ();

   --v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);

   open c1();
   loop
      fetch c1 into c1_row;
      exit when c1%notfound;

         p_clm_com_docs_index_tab.extend;

         p_clm_com_docs_index_tab (p_clm_com_docs_index_tab.count) :=
            customer.clm_com_docs_index_rec (
               c1_row.archive_no,
               c1_row.communication_no,
               c1_row.ext_reference,
               c1_row.document_code,
               c1_row.order_no,
               null,                                      --c1_row.index_date,
               null,                                      --c1_row.doc_sender,
               null,                                 --c1_row.department_code,
               c1_row.is_orginal,
               c1_row.invoice_date,
               c1_row.invoice_no,
               c1_row.invoice_amount,
               c1_row.swf_code,
               null,                                   --c1_row.expert_barkod,
               null,                                     --c1_row.expert_is_orginal,
               null,                                     --c1_row.explanation,
               null,                                  --c1_row.comm_user_name,
               c1_row.activepassive,                    --c1_row.selected,
               null,                                      --c1_row.is_scanned,
               NVL(c1_row.other_name, get_doc_name (c1_row.document_code)),
               is_doc_invoice (c1_row.document_code),
               nvl(C1_ROW.TRANSFER_DOC_ID,0),
               null                                      --c1_row.deliver_date,
            );
   end loop;

   close c1;
end;

   PROCEDURE GET_EKSPERT_LIST_BY_CODE (P_CODE NUMBER, P_EKSPERT_LIST OUT refcur)
   IS

    v_result   refcur;
   BEGIN
      OPEN v_result FOR
        SELECT   d.supp_id expert_code,
                 i.first_name || ' ' ||  i.surname expert_name
          FROM   clm_suppliers d,
                 cp_partners i,
                 koc_cp_partners_ext k,
                 alz_exp_branch_exp_rel a
         WHERE   d.exp_date IS NULL AND a.expert_supp_id = d.supp_id
                 AND NVL (a.validity_end_date, SYSDATE) =
                       (SELECT   MAX (NVL (dd.validity_end_date, SYSDATE))
                          FROM   alz_exp_branch_exp_rel dd
                         WHERE   dd.expert_supp_id = d.supp_id)
                 AND i.part_id = d.part_id
                 AND k.part_id = d.part_id
                 AND d.supp_id = P_CODE;

    P_EKSPERT_LIST:=  v_result;

   END;

   PROCEDURE GET_ASER_LIST_BY_CODE (P_CODE NUMBER, P_ASER_LIST OUT refcur)
   IS

    v_result   refcur;
   BEGIN
        OPEN v_result FOR
            SELECT   DISTINCT A.SUPP_ID, B.NAME
              FROM   CLM_SUPPLIERS A, CP_V_PARTNERS B
             WHERE   A.SUPP_TYPE = 'ASER'
                     AND A.PART_ID = B.PART_ID
                    AND A.SUPP_ID = P_CODE;

    P_ASER_LIST:=  v_result;

   END;

   PROCEDURE GET_EKSPERT_ASER_LIST_BY_TYPE (P_CODE NUMBER, P_TYPE VARCHAR2, P_LIST OUT refcur)
   IS

   BEGIN

        IF (NVL(P_TYPE, '01') = '02')
        THEN
            GET_ASER_LIST_BY_CODE(P_CODE, P_LIST);
        ELSE
            GET_EKSPERT_LIST_BY_CODE(P_CODE, P_LIST);
        END IF;

   END;


   PROCEDURE GET_DOC_LIST_BY_ARCHIVE_NO (
      P_Archive_No               IN     NUMBER,
      P_CLM_COM_DOCS_INDEX_TAB      OUT CLM_COM_DOCS_INDEX_TAB
   )
   IS
      CURSOR c1
      IS
         SELECT   ARCHIVE_NO,
                  COMMUNICATION_NO,
                  EXT_REFERENCE,
                  DOCUMENT_CODE,
                  ORDER_NO,
                  INDEX_DATE,
                  DELIVER_DATE,
                  DOC_SENDER,
                  DEPARTMENT_CODE,
                  IS_ORGINAL,
                  INVOICE_DATE,
                  INVOICE_NO,
                  INVOICE_AMOUNT,
                  SWF_CODE,
                  EXPERT_BARKOD,
                  EXPERT_IS_ORGINAL,
                  EXPLANATION,
                  COMM_USER_NAME,
                  IS_SCANNED,
                  ACTIVEPASSIVE,
                  OTHER_NAME,
                  TRANSFER_DOC_ID
           FROM   ALZ_CLM_COM_DOCS_INDEX_TBL
          WHERE   ARCHIVE_NO = P_Archive_No AND ACTIVEPASSIVE = 1;

      C1_ROW   C1%ROWTYPE;
   BEGIN
      p_CLM_COM_DOCS_INDEX_TAB := CLM_COM_DOCS_INDEX_TAB ();

      OPEN C1;

      LOOP
         FETCH C1 INTO C1_ROW;

         EXIT WHEN C1%NOTFOUND;

         p_CLM_COM_DOCS_INDEX_TAB.EXTEND;
         p_CLM_COM_DOCS_INDEX_TAB (p_CLM_COM_DOCS_INDEX_TAB.COUNT) :=
            customer.CLM_COM_DOCS_INDEX_REC (C1_ROW.ARCHIVE_NO,
                                             C1_ROW.COMMUNICATION_NO,
                                             C1_ROW.EXT_REFERENCE,
                                             C1_ROW.DOCUMENT_CODE,
                                             C1_ROW.ORDER_NO,
                                             C1_ROW.INDEX_DATE,
                                             C1_ROW.DOC_SENDER,
                                             C1_ROW.DEPARTMENT_CODE,
                                             C1_ROW.IS_ORGINAL,
                                             C1_ROW.INVOICE_DATE,
                                             C1_ROW.INVOICE_NO,
                                             C1_ROW.INVOICE_AMOUNT,
                                             C1_ROW.SWF_CODE,
                                             C1_ROW.EXPERT_BARKOD,
                                             C1_ROW.EXPERT_IS_ORGINAL,
                                             C1_ROW.EXPLANATION,
                                             C1_ROW.COMM_USER_NAME,
                                             C1_ROW.IS_SCANNED,
                                             C1_ROW.ACTIVEPASSIVE,
                                             NVL(C1_ROW.OTHER_NAME,GET_DOC_NAME (C1_ROW.DOCUMENT_CODE)),
                                              IS_DOC_INVOICE(C1_ROW.DOCUMENT_CODE),
                                              nvl(C1_ROW.TRANSFER_DOC_ID,0),
                                              C1_ROW.DELIVER_DATE
                                              );
      END LOOP;

      CLOSE C1;
   END;

   --GELEN EVRAK KAYIT EKRANI EKSIK EVRAK LISTESI POPUP
   PROCEDURE GET_ABSENT_DOC (p_EXT_REFERENCE     IN     VARCHAR2,
                             P_ABSENT_DOC_LIST      OUT refcur)
   IS
      v_result   refcur;
      v_ext_reference   VARCHAR2(100);

   BEGIN
      v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);
      OPEN v_result FOR
           SELECT   DISTINCT B.DOC_CODE,
                             C.EXPLANATION,
                             B.ENTRANCE_DATE,
                             B.EXPLANATION,
                             B.USERNAME
             FROM   CLM_SUBFILES A, KOC_CLM_DOC_INFO B, KOC_CLM_DOC_REF C
            WHERE       A.EXT_REFERENCE = v_ext_reference
                    AND A.CLAIM_ID = B.CLAIM_ID
                    AND A.SF_NO = B.SF_NO
                    AND B.DOC_CODE = C.DOC_CODE
                    AND ( (NVL (IS_RECEIVED, 0) = 1) OR (NVL (IS_VALID, 0) = 1))
         ORDER BY   ENTRANCE_DATE DESC;


      P_ABSENT_DOC_LIST := v_result;
   END;

   --gelen EVRAK KAYIRT SORGULAMA VE GÖRÜNTÜLEME evrak HAREKETLERI LISTESI popup
   PROCEDURE GET_DOC_MOVEMENTS (p_EXT_REFERENCE        IN     VARCHAR2,
                                P_DOC_MOVEMENTS_LIST      OUT refcur)
   IS
      v_result   refcur;
      v_ext_reference   VARCHAR2(100);
   BEGIN
      v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);
      OPEN v_result FOR
           SELECT   B.CSH_DATE, B.CSH_USERNAME, B.CSH_COMMENT
             FROM   CLM_SUBFILES A, CLM_STATUS_HISTORY B
            WHERE       A.EXT_REFERENCE = v_ext_reference
                    AND A.CLAIM_ID = B.CLAIM_ID
                    AND A.SF_NO = B.SF_NO
                    AND B.CLM_STATUS <> 'EKVT'
                    --AND B.CSH_USERNAME LIKE 'WMUH%'
         --  ((:P_USER_NAME IN ('WPOKYAY','WMGUNDOGDU')) OR
         -- (:P_USER_NAME NOT IN ('WPOKYAY','WMGUNDOGDU') AND
         -- B.CLM_STATUS NOT IN ('WEKVT','WPERTMAIL')))
         ORDER BY   B.CSH_DATE DESC, B.STATUS_ID DESC;



      P_DOC_MOVEMENTS_LIST := v_result;
   END;




    PROCEDURE check_ext_reference (
       p_ext_reference         IN       VARCHAR2,
       p_check_ext_reference   OUT      NUMBER,
       p_platform in varchar2 default 'DMS'
    )
    IS
       v_ext_reference   VARCHAR2 (100);

    BEGIN

       p_check_ext_reference := 0;
       v_ext_reference := get_ext_reference_by_rucu (p_ext_reference);

       SELECT 1
         INTO P_CHECK_EXT_REFERENCE
         FROM CLM_SUBFILES A, KOC_CLM_DETAIL B
        WHERE A.EXT_REFERENCE = V_EXT_REFERENCE
          AND A.CLAIM_ID = B.CLAIM_ID
          AND A.SF_NO = B.SF_NO
          AND B.DETAIL_NO = 1
          AND B.DENOUNCE_DATE >= ALZ_CLM_DMS_MAIN_UTILS.GETMILATDATE (A.SF_TYPE, p_platform);
    EXCEPTION
       WHEN OTHERS
       THEN
          p_check_ext_reference := 0;
    END;

   PROCEDURE CHECK_EXT_REFERENCE_LIST (
       p_ext_reference_list         IN     VARCHAR2,
       p_check_ext_reference_list   OUT    VARCHAR2,
       p_platform                   IN     VARCHAR2 DEFAULT 'DMS'
    )
    IS
       CURSOR cFiles
       IS
              SELECT TRIM (REGEXP_SUBSTR (p_ext_reference_list, '[^;]+', 1, LEVEL)) Ext_Reference
                FROM DUAL
            CONNECT BY
                REGEXP_SUBSTR (p_ext_reference_list, '[^;]+', 1, LEVEL) IS NOT NULL;

       cFilesRec   cFiles%ROWTYPE;
       --
       v_check     NUMBER (1);
       --
    BEGIN
       --
       OPEN cFiles;

       FETCH cFiles INTO cFilesRec;

       WHILE cFiles%FOUND
       LOOP
          IF cFilesRec.ext_reference IS NOT NULL
          THEN
             --DBMS_OUTPUT.Put_Line (cFilesRec.ext_reference);
             check_ext_reference (cFilesRec.ext_reference, v_check, 'DMS');

             IF v_check = 0
             THEN
                IF p_check_ext_reference_list IS NOT NULL
                THEN
                   p_check_ext_reference_list := p_check_ext_reference_list || ';';
                END IF;

                p_check_ext_reference_list := p_check_ext_reference_list || cFilesRec.ext_reference;
             END IF;
          END IF;

          FETCH cFiles INTO cFilesRec;
       END LOOP;
    --
    EXCEPTION
       WHEN OTHERS
       THEN
          p_check_ext_reference_list := p_ext_reference_list;
    END;

   PROCEDURE GET_INDEX_USER_LIST ( P_INDEX_USER_LIST      OUT refcur)
   IS
      v_result   refcur;
   BEGIN

    OPEN v_result FOR
    SELECT username comm_user_name
      FROM koc_auth_user_role_rel
     WHERE role_code in ('WDMSCMM','WDMSCLM','WDMSCMO','WDMSC_O','WDMSC_M')
       AND username like 'W%'
     GROUP BY username
     UNION ALL
    SELECT 'WDMOTOANALIZ' comm_user_name
      FROM dual
     ORDER BY 1;

    P_INDEX_USER_LIST := v_result;

   END;

    FUNCTION GET_CLM_CANCELLED_STATUS(P_EXT_REFERENCE VARCHAR2)
       RETURN NUMBER
    IS
       V   NUMBER := 0;
    BEGIN
       BEGIN
          SELECT 1
            INTO V
            FROM CLM_SUBFILES A
           WHERE A.EXT_REFERENCE = P_EXT_REFERENCE AND A.CLM_STATUS = 'CANCELLED';
       EXCEPTION
          WHEN OTHERS
          THEN
             V := 0;
       END;

       RETURN (V);
    END;

    FUNCTION GET_CLM_CLOSED_STATUS(P_EXT_REFERENCE VARCHAR2)
       RETURN NUMBER
    IS
       V   NUMBER := 0;
    BEGIN
       BEGIN
          SELECT 1
            INTO V
            FROM CLM_SUBFILES A
           WHERE A.EXT_REFERENCE = P_EXT_REFERENCE AND A.CLM_STATUS = 'CLOSED';
       EXCEPTION
          WHEN OTHERS
          THEN
             V := 0;
       END;

       RETURN (V);
    END;

PROCEDURE GET_COURT_LIST(cur OUT refcur,
                         p_process_results   OUT     customer.process_result_table)
   IS
      v_result   refcur;
   BEGIN
      OPEN v_result FOR
            SELECT COURT_NAME,COURT_ID,COURT_TYPE
            FROM ALZ_LAW_COURTS_V
            ORDER BY COURT_NAME;

        -- SELECT DESCRIPTION,COURT_ENF_NO,COURT_ENF_TYPE,COURT_TYPE
        --   FROM KOC_LAW_COURT_REF
        --  WHERE COURT_ENF_TYPE NOT IN ('S');


      cur := v_result;

      EXCEPTION
    WHEN OTHERS THEN
        alz_web_process_utils.process_result (
            0,
            9,
            -1,
            'GET_COURT_LIST',
            'Mahkeme Listesi Alinamadi.',
            'Mahkeme Listesi Alinamadi',
            NULL,
            NULL,
            'ALZ_CLM_DMS_UTILS.GET_COURT_LIST',
            NULL,
            p_process_results);

   END;

PROCEDURE DOC_TYPE_COM_LIST_BY_LAW_FILE (
   p_law_file_no              IN   VARCHAR2,
   p_communication_no         IN   NUMBER,
   p_clm_com_docs_index_tab   IN OUT clm_com_docs_index_tab,
   p_process_results          IN OUT customer.process_result_table

)

is

   v_ext_reference  VARCHAR2(100);

   cursor c1 is

SELECT   a.archive_no,
         a.communication_no,
         M.LAW_FILE_NO,
         a.document_code,
         a.order_no,
         a.is_orginal,
         a.invoice_amount,
         a.swf_code,
         a.invoice_date,
         a.invoice_no,
         a.activepassive,
         a.other_name,
         A.TRANSFER_DOC_ID
  FROM   alz_clm_com_docs_index_tbl A,alz_law_com_tbl M
 WHERE  ((M.LAW_FILE_NO = p_law_file_no) OR (p_law_file_no IS NULL AND M.LAW_FILE_NO IS NULL))
         AND a.communication_no = p_communication_no
         AND M.ARCHIVE_NO = A.ARCHIVE_NO
         AND M.COMMUNICATION_NO =A.COMMUNICATION_NO;

   c1_row           c1%rowtype;
begin

   p_clm_com_docs_index_tab := clm_com_docs_index_tab ();

   --v_ext_reference := get_ext_reference_by_rucu(p_ext_reference);

   open c1();
   loop
      fetch c1 into c1_row;
      exit when c1%notfound;

         p_clm_com_docs_index_tab.extend;

         p_clm_com_docs_index_tab (p_clm_com_docs_index_tab.count) :=
            customer.clm_com_docs_index_rec (
               c1_row.archive_no,
               c1_row.communication_no,
               c1_row.LAW_FILE_NO,
               c1_row.document_code,
               c1_row.order_no,
               null,                                      --c1_row.index_date,
               null,                                      --c1_row.doc_sender,
               null,                                 --c1_row.department_code,
               c1_row.is_orginal,
               c1_row.invoice_date,
               c1_row.invoice_no,
               c1_row.invoice_amount,
               c1_row.swf_code,
               null,                                   --c1_row.expert_barkod,
               null,                                     --c1_row.expert_is_orginal,
               null,                                     --c1_row.explanation,
               null,                                  --c1_row.comm_user_name,
               c1_row.activepassive,                    --c1_row.selected,
               null,                                      --c1_row.is_scanned,
               NVL(c1_row.other_name, get_law_doc_name (c1_row.document_code)),
               is_doc_invoice (c1_row.document_code),
               nvl(C1_ROW.TRANSFER_DOC_ID,0),
               null                                      --c1_row.deliver_date,
            );
   end loop;

   close c1;
end;

FUNCTION get_law_doc_name (p_doc_code IN VARCHAR2)
   RETURN VARCHAR2
IS
   v_result   VARCHAR2 (500);
BEGIN
   SELECT   DOC_DESC
      INTO   v_result
     FROM   ALZ_LAW_DOC_TYPE_DEF a
    WHERE   ltrim(a.CODE,'0') =ltrim(p_doc_code ,'0');

   RETURN v_result;

    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN null;

END;

PROCEDURE GET_DEPT_DOC_REL(P_DEPT_DOC_REL OUT SYS_REFCURSOR) is
   begin
     OPEN P_DEPT_DOC_REL FOR
          SELECT A.DEPARTMENT_CODE, A.DOC_CODE, A.DOC_ORDER FROM ALZ_CLM_DEPT_DOC_REL A;
   end;

END;
/

