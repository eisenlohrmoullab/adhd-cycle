* Encoding: UTF-8.

**************************************.
Original Syntax by: Ashley Eng.
**************************************.

COMPUTE CSS_B_TotalIASymp=CSS_B_1 + CSS_B_3 + CSS_B_5 + CSS_B_7 + CSS_B_9 + CSS_B_11 + CSS_B_13 + 
    CSS_B_15 + CSS_B_17.
VARIABLE LABELS CSS_B_TotalIASymp 'items 1, 3, 5, 7, 9. 11. 13. 15 .17'. 
EXECUTE.

COMPUTE CSS_B_TotalHISymp=CSS_B_2 + CSS_B_4 + CSS_B_6 + CSS_B_8 + CSS_B_10 + CSS_B_12 + CSS_B_14 + 
    CSS_B_16 + CSS_B_18.
VARIABLE LABELS CSS_B_TotalHISymp 'items 2 4 6 8 10 12 14 16 18'. 
EXECUTE.

COMPUTE CSS_TotalFunction=CSS_Function_1 + CSS_Function_2 + CSS_Function_3 + CSS_Function_4 + CSS_Function_5 + CSS_Function_6 + CSS_Function_7 + 
    CSS_Function_8 + CSS_Function_9 + CSS_Function_10.
EXECUTE.

COMPUTE CSS_B2_Total=CSS_B2_1 + CSS_B2_2 + CSS_B2_3 + CSS_B2_4 + CSS_B2_5 + CSS_B2_6 + CSS_B2_7 + 
    CSS_B2_8.
EXECUTE.

COMPUTE DEBQ_Total=DEBQ_1 + DEBQ_2 + DEBQ_3 + DEBQ_4 + DEBQ_5 + DEBQ_6 + DEBQ_7 + DEBQ_8 + DEBQ_9 + 
    DEBQ_10 + DEBQ_11 + DEBQ_12 + DEBQ_13.
EXECUTE.

COMPUTE BDEFS_Total=BDEFS_1 + BDEFS_2 + BDEFS_3 + BDEFS_4 + BDEFS_5 + BDEFS_6.
EXECUTE.

RECODE UPPS_7 UPPS_8 (1=4) (2=3) (3=2) (4=1).
EXECUTE.

COMPUTE UPPS_Total=UPPS_1 + UPPS_2 + UPPS_3 + UPPS_4 + UPPS_5 + UPPS_6 + UPPS_7 + UPPS_8 + UPPS_9 + 
    UPPS_10 + UPPS_11 + UPPS_12 + UPPS_13 + UPPS_14 + UPPS_15.
VARIABLE LABELS UPPS_Total 'After recoding items 7 and 8'.
EXECUTE.


**********************************.
DATE: 4/25/2021.
Syntax By: Pevitr S. Bansal.
**********************************.

Recoding the RecipientLastName Variable (STRING) into a (NUMERIC) StudyID variable.
Need a StudyID variable to merge all datasets.

RECODE RecipientLastName (CONVERT) INTO StudyID.
EXECUTE. 
