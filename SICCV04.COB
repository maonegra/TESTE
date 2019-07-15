       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SICCV04.
       AUTHOR.       FRANCISCO
       INSTALLATION. HBSIS.
       DATE-WRITTEN. 14/07/2019.
       SECURITY.
      *
      ***********************************************************************
      * SISTEMA......: SICCV – SISTEMA DE CONTROLE DE VENDEDOR POR VENDEDOR *
      ***********************************************************************
      * ANALISTA.....: FRANCISCO A. ROLIM DE MOURA JR                       *
      * LINGUAGEM....: COBOL                                                *
      * PROGRAMADOR..: FRANCISCO A. ROLIM DE MOURA JR                       *
      * DATA.........: 14/07/2019                                           *
      ***********************************************************************
      * OBJETIVO.....: SICCV04 - RELATORIO DE VENDEDORES                    *
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY VENDEDOR.SEL.
     |     SELECT LISTAG ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       COPY VENDEDOR.FD.
     | FD  LISTAG LABEL RECORD IS OMITTED.
     | 01  LINHA PIC X(230).
       WORKING-STORAGE SECTION.
       01  lnk-Status 		pic 9(001).
       01  lnk-letra  		pic x(100).
       01  w-letra    		pic x(100).
       01  w-tam      		pic 9(003).
       01  w-fim      		pic 9(003).
       01  w-ind      		pic 9(003).
       01  W-ROW                PIC S9(8) COMP-5.
       01  W-COL                PIC S9(8) COMP-5.
       01  IX                   PIC 9(2) VALUE 0.
       01  IY                   PIC 9(2) VALUE 0.
       01  LIMITE               PIC 9(2) VALUE 0.
       01  CW-NOME              PIC X(40) VALUE SPACES.
       01  WS-NOME              PIC X(40) VALUE SPACES.
       01  F REDEFINES WS-NOME.
           05 WN	   PIC X(01) OCCURS 40 TIMES.
       01  WX-NOME    PIC X(40) VALUE SPACES.
       01  F REDEFINES WX-NOME.
           05 WX	   PIC X(01) OCCURS 40 TIMES.


       01  FLAG                 PIC X(01) VALUE SPACES.
       01  FS-STAT              PIC 9(02) VALUE ZEROS.
       01  WS-OPCAO             PIC 9(01) VALUE ZEROS.
       01  WS-MSGERRO           PIC X(43).
       01  WS-MASCARA           PIC ZZ9,99999999 VALUE ZEROS.
       01  WS-RESPOSTA          PIC X(01) VALUE SPACES.
       01  XX-VD-NOME          PIC X(40).
       01  XX-VD-CNPJ           PIC 9(14).
       01  WS-VD-AD             PIC X.
       01  WS-VD-NA             PIC X.
       01  WS-VD-VENDEDOR       PIC 9(3).
       01  WS-REG-VENDEDOR.
           05 WS-VD-CODIGO      PIC 9(03).
           05 WS-CODIGO REDEFINES WS-VD-CODIGO.
              10 RE-CODIGO      PIC 9 OCCURS 3 TIMES.
           05 WS-VD-NOME       PIC X(40).
           05 WS-NOME   REDEFINES WS-VD-NOME.
              10 RE-NOME        PIC X OCCURS 40 TIMES.
           05 WS-VD-CHAVE-CPF.
              10 WS-VD-CPF     PIC 9(14).
              10 WS-CPF REDEFINES WS-VD-CPF.
                 10 RE-CPF      PIC 9 OCCURS 14 TIMES.
           05 WS-VD-LATITUDE    PIC S9(03)V9(8).
           05 WS-LATITUDE REDEFINES WS-VD-LATITUDE.
              10 RE-LATITUDE    PIC 9 OCCURS 11 TIMES.
           05 WS-VD-LONGITUDE   PIC S9(03)V9(8).
           05 WS-LONGITUDE REDEFINES WS-VD-LONGITUDE.
              10 RE-LONGITUDE   PIC 9 OCCURS 11 TIMES.
       01  WS-ARQUIVO-LST.
           05 AR-NOME    PIC X(05) VALUE 'COBSP'.
           05 AR-COD     PIC X(01) VALUE '1'.
           05 AR-RESTO   PIC X(04) VALUE '.LST'.
       01  LINHA-01              PIC X(80) VALUE ALL '-'.
       01  LINHA-02.
           05 F                  PIC X(20) VALUE SPACES.
           05 F                  PIC X(50) VALUE
              'L I S T A G E M    D E   V E D E D O R E S'.
       01  LINHA-03              PIC X(80) VALUE " CODIGO            NOM
      -    "ME VENDEDOR         C.P.F       LATITUDE    LONGITUDE     ".
       01  LINHA-04              PIC X(80) VALUE "------- --------------
      -    "------------------ -------------- ----------- ----------- ".
       01  LINHA05.
           05 L05-CODIGO         PIC ZZZZZZZ VALUE ZEROS.
           05 F                  PIC X VALUE ' '.
           05 L05-NOME           PIC X(32)   VALUE SPACES.
           05 F                  PIC X VALUE ' '.
           05 L05-CPF            PIC ZZ.ZZZ.ZZZBZZZZBZZ VALUE ZEROS.
           05 F                  PIC X VALUE ' '.
           05 L05-LA             PIC ZZ,ZZZZZZZZ VALUE ZEROS.
           05 F                  PIC X VALUE ' '.
           05 L05-LO             PIC ZZ,ZZZZZZZZ VALUE ZEROS.


       SCREEN SECTION.
       01  SS-JANELA.
           02  BLANK SCREEN.
           02  LINE 1  COLUMN 1  VALUE
               "ษอออออออออออออออออออออออออออออออออออออออ".
           02  LINE 1  COLUMN 41 VALUE
               "อออออออออออออออออออออออออออออออออออออออป".
           02  LINE 2  COLUMN 1  VALUE "บ".
           02  LINE 2  COLUMN 80 VALUE "บ".
           02  LINE 3  COLUMN 1  VALUE
               "ฬอออออออออออออออออออออออออออออออออออออออ".
           02  LINE 3  COLUMN 41 VALUE
               "อออออออออออออออออออออออออออออออออออออออน".
           02  LINE 4  COLUMN 1  VALUE "บ".
           02  LINE 4  COLUMN 80 VALUE "บ".
           02  LINE 5  COLUMN 1  VALUE "บ".
           02  LINE 5  COLUMN 80 VALUE "บ".
           02  LINE 6  COLUMN 1  VALUE "บ".
           02  LINE 6  COLUMN 80 VALUE "บ".
           02  LINE 7  COLUMN 1  VALUE "บ".
           02  LINE 7  COLUMN 80 VALUE "บ".
           02  LINE 8  COLUMN 1  VALUE "บ".
           02  LINE 8  COLUMN 80 VALUE "บ".
           02  LINE 9  COLUMN 1  VALUE "บ".
           02  LINE 9  COLUMN 80 VALUE "บ".
           02  LINE 10 COLUMN 1  VALUE "บ".
           02  LINE 10 COLUMN 80 VALUE "บ".
           02  LINE 11 COLUMN 1  VALUE "บ".
           02  LINE 11 COLUMN 80 VALUE "บ".
           02  LINE 12 COLUMN 1  VALUE "บ".
           02  LINE 12 COLUMN 80 VALUE "บ".
           02  LINE 13 COLUMN 1  VALUE "บ".
           02  LINE 13 COLUMN 80 VALUE "บ".
           02  LINE 14 COLUMN 1  VALUE "บ".
           02  LINE 14 COLUMN 80 VALUE "บ".
           02  LINE 15 COLUMN 1  VALUE "บ".
           02  LINE 15 COLUMN 80 VALUE "บ".
           02  LINE 16 COLUMN 1  VALUE "บ".
           02  LINE 16 COLUMN 80 VALUE "บ".
           02  LINE 17 COLUMN 1  VALUE "บ".
           02  LINE 17 COLUMN 80 VALUE "บ".
           02  LINE 18 COLUMN 1  VALUE "บ".
           02  LINE 18 COLUMN 80 VALUE "บ".
           02  LINE 19 COLUMN 1  VALUE "บ".
           02  LINE 19 COLUMN 80 VALUE "บ".
           02  LINE 20 COLUMN 1  VALUE "บ".
           02  LINE 20 COLUMN 80 VALUE "บ".
           02  LINE 21 COLUMN 1  VALUE "บ".
           02  LINE 21 COLUMN 80 VALUE "บ".
           02  LINE 22 COLUMN 1  VALUE
               "ฬออออออออออหออออออออออออออออออออออออออออ".
           02  LINE 22 COLUMN 41 VALUE
               "อออออออออออออออหออออออออออหออออออออออออน".
           02  LINE 23 COLUMN 1  VALUE "บ MENSAGEM บ".
           02  LINE 23 COLUMN 56 VALUE "บ".
           02  LINE 23 COLUMN 67 VALUE "บ".
           02  LINE 23 COLUMN 80 VALUE "บ".
           02  LINE 24 COLUMN 1  VALUE
               "ศออออออออออสออออออออออออออออออออออออออออ".
           02  LINE 24 COLUMN 41 VALUE
               "อออออออออออออออสออออออออออสออออออออออออผ".
           02  LINE 2  COLUMN 25 VALUE
               "*** LISTAGEM DE VENDEDOR ***".
       01  SS-CABEC-LISTAGEM.
           02 LINE 04 COLUMN 21 VALUE
              "L I S T A G E M    D E   V E N D E D O R E S".
       01  SS-MENU.
           02 LINE 07 COLUMN 20 VALUE
              "CODIGO ESPECอFICO DO VENDEDOR..........:        ".
           02 LINE 08 COLUMN 20 VALUE
              "NOME VENDEDOR(LISTAGEM POR HOMONIMO)...:        ".
           02 LINE 09 COLUMN 20 VALUE
              "                                                ".
           02 LINE 11 COLUMN 20 VALUE
              "ORDEM ASCENDENTE OU DESCENDENTE ? (A/D):        ".
           02 LINE 12 COLUMN 20 VALUE
              "NUMERICA OU ALFABETICA ?..........(N/A):        ".
       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO WS-OPCAO
           PERFORM ABRIR-ARQUIVOS.
       PROCESSA.
           DISPLAY SS-JANELA
           DISPLAY SS-MENU
           ACCEPT SS-MENU.
           PERFORM SEGUE-01 UNTIL WS-OPCAO = 9.
           CHAIN 'SICCV'
           STOP RUN.
       SEGUE-01.
           MOVE ' ' TO FLAG
           ACCEPT(07 63) WS-VD-CODIGO WITH PROMPT
           ACCEPT(08 63) WS-VD-NOME WITH PROMPT
           ACCEPT(11 63) WS-VD-AD WITH PROMPT
           ACCEPT(12 63) WS-VD-NA WITH PROMPT
           IF WS-VD-CODIGO > 0
              MOVE SPACES TO WS-VD-NOME
              DISPLAY(08 63) WS-VD-NOME
              DISPLAY(11 63) ' '
              DISPLAY(12 63) ' '
              MOVE '1' TO FLAG.
           IF WS-VD-NOME NOT = SPACES
              MOVE 0 TO WS-VD-CODIGO
              DISPLAY(08 63) '       '
              MOVE '2' TO FLAG.
           IF WS-VD-AD NOT = 'A' AND 'D'
              IF WS-VD-CODIGO = 0
                 MOVE 'A' TO WS-VD-AD
                 DISPLAY(11 63) WS-VD-AD
                 move '3' TO FLAG.
           IF WS-VD-NA NOT = 'N' AND 'A'
              IF WS-VD-CODIGO = 0
                 MOVE 'N' TO WS-VD-NA
                 DISPLAY(11 63) WS-VD-NA
                 move '3' TO FLAG.
           IF FLAG = '1'
              PERFORM LISTA-1 UNTIL WS-OPCAO = 9.
           IF FLAG = '2'
              PERFORM LISTA-2 UNTIL WS-OPCAO = 9.
           IF FLAG = '3'
              PERFORM LISTA-3 UNTIL WS-OPCAO = 9.
       LISTA-1.
           MOVE WS-VD-CODIGO TO VD-CODIGO
           READ VENDEDOR
           IF FS-STAT NOT = 00
              MOVE 'VENDEDOR NAO ENCONTRADO' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
           ELSE
              PERFORM LISTA-VENDEDOR.
           MOVE 9 TO WS-OPCAO.
       LISTA-VENDEDOR.
           PERFORM MOVE-DADOS
           PERFORM CABEC
           PERFORM IMPRIME
           WRITE LINHA FROM LINHA-04 BEFORE PAGE
           MOVE 9 TO WS-OPCAO.
       LISTA-2.
           READ VENDEDOR KEY IS VD-NOME
           move WS-VD-NOME to lnk-letra
           move 40 to w-tam
           perform busca-por-digitacao until w-tam = 0
           COMPUTE W-FIM = 50 - W-TAM
           move lnk-letra(1 : w-tam) to w-letra
           move w-letra to VD-NOME

           IF WS-VD-AD = 'A'
              START VENDEDOR KEY NOT LESS THAN VD-NOME
           ELSE
              START VENDEDOR KEY NOT > THAN VD-NOME.
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              PERFORM CABEC
              IF WS-VD-AD = 'A'
                 PERFORM IMPRIME-HOMONIMO-A UNTIL WS-OPCAO = 9
              ELSE
                 PERFORM IMPRIME-HOMONIMO-D UNTIL WS-OPCAO = 9.
       IMPRIME-HOMONIMO-A.
           READ VENDEDOR NEXT AT END
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              move 1 to w-ind
              perform ve-homonome until ws-opcao = 8.
       IMPRIME-HOMONIMO-D.
           READ VENDEDOR PREVIOUS AT END
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              move 1 to w-ind
              perform ve-homonome until ws-opcao = 8.
       VE-HOMONOME.
           if lnk-letra(1:w-tam) NOT = SPACES
              if lnk-letra(1:w-tam) = VD-NOME(W-IND:w-tam)
                 PERFORM MOVE-DADOS
                 PERFORM IMPRIME
              ELSE
                 add 1 to w-ind
                 IF W-IND > W-FIM
                    move 8 to ws-opcao.
       busca-por-digitacao.
           if lnk-letra(w-tam:1) = spaces
              SUBTRACT 1 FROM w-tam.

       LISTA-3.
           IF WS-VD-AD = 'A' AND WS-VD-NA = 'N'
              CLOSE VENDEDOR
              OPEN I-O VENDEDOR.
           IF WS-VD-AD = 'A' AND WS-VD-NA = 'A'
              READ VENDEDOR KEY IS VD-NOME
              MOVE ' ' TO VD-NOME
              START VENDEDOR KEY NOT LESS THAN VD-NOME.
           IF WS-VD-AD = 'D' AND WS-VD-NA = 'N'
              MOVE 9999999 TO WS-VD-CODIGO
              START VENDEDOR KEY NOT LESS THAN VD-CODIGO.
           IF WS-VD-AD = 'D' AND WS-VD-NA = 'A'
              READ VENDEDOR KEY IS VD-NOME
              MOVE  'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ' 
              TO VD-NOME
              START VENDEDOR KEY NOT > THAN VD-NOME.
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              PERFORM CABEC
              IF WS-VD-AD = 'A'
                 PERFORM IMPRIME-VENDEDOR-A UNTIL WS-OPCAO = 9
                 WRITE LINHA FROM LINHA-04 BEFORE PAGE
              ELSE
                 PERFORM IMPRIME-VENDEDOR-D UNTIL WS-OPCAO = 9
                 WRITE LINHA FROM LINHA-04 BEFORE PAGE.
       IMPRIME-VENDEDOR-A.
           READ VENDEDOR NEXT AT END
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              IF VD-VENDEDOR = WS-VD-VENDEDOR
                 PERFORM MOVE-DADOS
                 PERFORM IMPRIME.
       IMPRIME-VENDEDOR-D.
           READ VENDEDOR PREVIOUS
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              IF VD-VENDEDOR = WS-VD-VENDEDOR
                 PERFORM MOVE-DADOS
                 PERFORM IMPRIME.
       LISTA-4.
           CLOSE VENDEDOR
           OPEN I-O VENDEDOR.
           PERFORM CABEC
           PERFORM IMPRIME-TUDO-AN UNTIL WS-OPCAO = 9
           WRITE LINHA FROM LINHA-04 BEFORE PAGE.
       IMPRIME-TUDO-AN.
           READ VENDEDOR NEXT AT END
           IF FS-STAT NOT = 00
              MOVE 9 TO WS-OPCAO
           ELSE
              PERFORM MOVE-DADOS
              PERFORM IMPRIME.
       MOVE-DADOS.
           MOVE VD-CODIGO    TO L05-CODIGO
           MOVE VD-NOME      TO L05-NOME
           MOVE VD-CNPJ      TO L05-CNPJ
           MOVE VD-LATITUDE  TO L05-LA
           MOVE VD-LONGITUDE TO L05-LO.
       CABEC.
           WRITE LINHA FROM LINHA01 BEFORE 1
           WRITE LINHA FROM LINHA02 BEFORE 1
           WRITE LINHA FROM LINHA01 BEFORE 2
           WRITE LINHA FROM LINHA03 BEFORE 1
           WRITE LINHA FROM LINHA04 BEFORE 1.
       IMPRIME.
           WRITE LINHA FROM LINHA05 BEFORE 1.
      * -----------------------------------
      * ABRE ARQUIVOS
       ABRIR-ARQUIVOS.
           OPEN I-O VENDEDOR
           IF FS-STAT = '30'
               DISPLAY(23 14) "ARQUIVO VENDEDOR NAO EXISTE"
               PERFORM TEMPO 10000 TIMES
               DISPLAY(23 14) '                           '.
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DO ARQUIVO: " FS-STAT
               PERFORM TEMPO 10000 TIMES
               DISPLAY(23 14) '                           '.
           OPEN OUTPUT LISTAG.
      * -----------------------------------
      * MOSTRA TELA DO REGISTRO SEM INFORMACOES
       MOSTRA-TELA-REGISTRO.
           DISPLAY SS-JANELA.
      * -----------------------------------
      * MOSTRA MENSAGEM, ESPERA ENTER, ATUALIZA BARRA STATUS
       MOSTRA-ERRO.
           DISPLAY(23 14) WS-MSGERRO
           PERFORM TEMPO 10000 TIMES
           MOVE SPACES TO WS-MSGERRO.

      * -----------------------------------
       TEMPO.
           DISPLAY(23 13) " ".


