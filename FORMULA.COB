       IDENTIFICATION DIVISION.
       PROGRAM-ID.   FORMULA.
       AUTHOR.       FRANCISCO
       INSTALLATION. HBSIS.
       DATE-WRITTEN. 14/07/2019.
       SECURITY.
      *
      ***********************************************************************
      * SISTEMA......: SICCV – SISTEMA DE CONTROLE DE CLIENTES POR VENDEDOR *
      ***********************************************************************
      * ANALISTA.....: FRANCISCO A. ROLIM DE MOURA JR                       *
      * LINGUAGEM....: COBOL                                                *
      * PROGRAMADOR..: FRANCISCO A. ROLIM DE MOURA JR                       *
      * DATA.........: 11/07/2019                                           *
      ***********************************************************************
      * OBJETIVO.....: FORMULA PARA COMPOR DISTANCIA                        *
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  G2                   PIC  9(01)V9(4) VALUE 3,1416.

       01  COSV-LA                PIC S9(03)V9(8) VALUE ZEROS.
       01  COSC-LA                PIC S9(03)V9(8) VALUE ZEROS.
       01  COSSENOS             PIC S9(03)V9(8) VALUE ZEROS.

       01  SENV-LA                PIC S9(03)V9(8) VALUE ZEROS.
       01  SENC-LA                PIC S9(03)V9(8) VALUE ZEROS.
       01  SENOS                PIC S9(03)V9(8) VALUE ZEROS.

       01  COSC-LO3               PIC S9(03)V9(8) VALUE ZEROS.
       01  SENC-LO3               PIC S9(03)V9(8) VALUE ZEROS.
       01  ACOSSENO             PIC S9(03)V9(8) VALUE ZEROS.

       LINKAGE SECTION.
       01  C-LA                   PIC S9(03)V9(8).
       01  C-LO                   PIC S9(03)V9(8).
       01  V-LA                   PIC S9(03)V9(8).
       01  V-LO                   PIC S9(03)V9(8).
       01  DISTANCIA            PIC  9(05)V9(3).

       PROCEDURE DIVISION C-LA C-LO V-LA V-LO DISTANCIA.
       ACHA-COSSENO.
           COMPUTE COSV-LA = (G2 * (90 - V-LA) / 180)
           CALL "CBL_COS" USING COSV-LA.
           COMPUTE COSC-LA = (G2 * (90 - C-LA) / 180)
           CALL "CBL_COS" USING COSC-LA.
           COMPUTE COSSENOS = COSV-LA * COSC-LA.
       ACHA-SENO.
           COMPUTE SENV-LA = (G2 * (90 - V-LA) / 180)
           CALL "CBL_SIN" USING SENV-LA.
           COMPUTE SENC-LA = (G2 * (90 - C-LA) / 180)
           CALL "CBL_SIN" USING SENC-LA.
           COMPUTE SENOS = SENV-LA * SENC-LA.
       ACHA-COSSENO-C-LO-V-LO.
           COMPUTE COSC-LO3 = ((C-LO - V-LO) * G2 / 180)
           CALL "CBL_COS" USING COSC-LO3.
       MULTIPLICA-SENOS-COSSENOC-LO3.
           COMPUTE SENC-LO3 = SENOS * COSC-LO3.
       SOMA-COSSENOS-SENOS-ES.
           COMPUTE ACOSSENO = COSSENOS + SENC-LO3.
       ACHA-ACOSSENO-SOMA-CS.
           CALL "CBL_ACOS" USING ACOSSENO.
       ACHA-DISTANCIA.
           COMPUTE DISTANCIA = 6371 * ACOSSENO.
