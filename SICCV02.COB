       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SICCV02.
       AUTHOR.       FRANCISCO
       INSTALLATION. HBSIS.
       DATE-WRITTEN. 13/07/2019.
       SECURITY.
      *
      ***********************************************************************
      * SISTEMA......: SICCV – SISTEMA DE CONTROLE DE VENDEDOR POR VENDEDOR *
      ***********************************************************************
      * ANALISTA.....: FRANCISCO A. ROLIM DE MOURA JR                       *
      * LINGUAGEM....: COBOL                                                *
      * PROGRAMADOR..: FRANCISCO A. ROLIM DE MOURA JR                       *
      * DATA.........: 10/07/2019                                           *
      ***********************************************************************
      * OBJETIVO.....: SICCV02 - CRUD DE VENDEDORES                         *
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENCSV ASSIGN TO DISK
                  FILE STATUS FS-STAT
                  ORGANIZATION IS LINE SEQUENTIAL.
       COPY VENDEDOR.SEL.

       DATA DIVISION.
       FILE SECTION.
       FD  VENCSV   LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "vendedor.csv".
       01  REG-VENCSV.
           05 VS-LINHA          PIC X(01) OCCURS 79 TIMES.
       COPY VENDEDOR.FD.
       WORKING-STORAGE SECTION.
       01  IDX-COD              PIC 9(02) VALUE ZEROS.
       01  IDX                  PIC 9(02) VALUE ZEROS.
       01  CT                   PIC 9(02) VALUE ZEROS.
       01  FS-STAT              PIC 9(02) VALUE ZEROS.
       01  WS-OPCAO             PIC 9(01) VALUE ZEROS.
       01  WS-MSGERRO           PIC X(43).
       01  WS-MASCARA           PIC Z.ZZZ.ZZZBZ  VALUE ZEROS.
       01  WS-RESPOSTA          PIC X(01) VALUE SPACES.
       01  XX-VD-NOME          PIC X(40).
       01  XX-VD-CPF           PIC 9(14).
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
               "*** CADASTRO DE VENDEDOR ***".
       01  SS-CABEC-INCLUSAO.
           02 LINE 04 COLUMN 21 VALUE
              "I N C L U S A O    D E   V E N D E D O R  ".
       01  SS-CABEC-IMPORTAR.
           02 LINE 04 COLUMN 21 VALUE
              "   I M P O R T A R    O    V E N D E D O R".
       01  SS-CABEC-ALTERACAO.
           02 LINE 04 COLUMN 21 VALUE
              "A L T E R A C A O   D E   V E N D E D O R ".
       01  SS-CABEC-EXCLUSAO.
           02 LINE 04 COLUMN 21 VALUE
              "E X C L U S A O    D E   V E N D E D O R  ".
       01  SS-MENU.
           02 LINE 07 COLUMN 33         VALUE "1 - INCLUIR".
           02 LINE 08 COLUMN 33         VALUE "2 - IMPORTAR".
           02 LINE 09 COLUMN 33         VALUE "3 - ALTERAR".
           02 LINE 10 COLUMN 33         VALUE "4 - EXCLUIR".
           02 LINE 11 COLUMN 33         VALUE "9 - ENCERRAR".
           02 LINE 13 COLUMN 33         VALUE "OPCAO: ".
           02  ss-opcao LINE 13 COLUMN 40 PIC 9 USING WS-OPCAO AUTO.
       01  SS-TELA-REGISTRO.
           02 LINE 10 COLUMN 12 VALUE "C๓digo do Vendedor:
      -       "                           ".
           02 LINE 12 COLUMN 12 VALUE "Nome do Vendedor.:
      -       "                           ".
           02 LINE 14 COLUMN 12 VALUE "C.P.F.............:
      -       "                           ".
           02 LINE 16 COLUMN 12 VALUE "Latitude..........:
      -       "                           ".
           02 LINE 18 COLUMN 12 VALUE "Longitude.........:
      -       "                           ".
       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO WS-OPCAO
           PERFORM ABRIR-ARQUIVOS
           PERFORM PROCESSA THRU SEGUE-01 UNTIL WS-OPCAO = 9
                   GO FINALIZA.
       PROCESSA.
           DISPLAY SS-JANELA
           DISPLAY SS-MENU
           ACCEPT SS-MENU.
       SEGUE-01.
           MOVE 1     TO WS-VD-CODIGO
           MOVE ZEROS TO WS-VD-CPF
                         WS-VD-LATITUDE WS-VD-LONGITUDE
           MOVE SPACES TO WS-VD-NOME.
           IF WS-OPCAO = 1
              PERFORM INCLUI UNTIL WS-OPCAO = 0.
           IF WS-OPCAO = 2
              PERFORM IMPORTAR UNTIL WS-OPCAO = 0.
           IF WS-OPCAO = 3
              PERFORM ALTERA UNTIL WS-OPCAO = 0.
           IF WS-OPCAO = 4
              PERFORM EXCLUI UNTIL WS-OPCAO = 0.
           IF WS-OPCAO > 4 AND WS-OPCAO < 9
              DISPLAY(23 14) "Opcao Invalida !!!"
              PERFORM TEMPO 10000 TIMES
              DISPLAY(23 14) "                  "
              MOVE 0 TO WS-OPCAO.
           MOVE 0 TO FS-STAT.
      * -----------------------------------
       INCLUI.
           PERFORM MOSTRA-TELA-REGISTRO
           DISPLAY SS-CABEC-INCLUSAO.
           IF WS-OPCAO = 1
              PERFORM INCLUI-CODIGO
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 1
              MOVE 0 TO FS-STAT
              PERFORM INCLUI-NOME
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 1
              MOVE 0 TO FS-STAT
              PERFORM INCLUI-CPF
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 1
              PERFORM INCLUI-SEGUE-RESTO THRU INCLUI-GRAVAR
                      UNTIL FS-STAT = 00.
       INCLUI-CODIGO.
           ACCEPT(10 32) WS-VD-CODIGO WITH PROMPT
           IF WS-VD-CODIGO > 0
              MOVE WS-VD-CODIGO TO VD-CODIGO
              READ VENDEDOR
              IF FS-STAT = 00
                 PERFORM MOSTRA-DADOS-VENDEDOR
                 MOVE 'VENDEDOR JA CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-VD-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-INCLUSAO
              ELSE
                 MOVE 1 TO WS-OPCAO
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       INCLUI-NOME.
           IF WS-OPCAO = 1
              ACCEPT(12 32) WS-VD-NOME WITH PROMPT
              MOVE WS-VD-NOME TO VD-NOME
              READ VENDEDOR KEY IS VD-NOME
              START VENDEDOR KEY IS EQUAL TO VD-NOME
              IF WS-VD-NOME = SPACES
                 MOVE 0 TO FS-STAT
                 MOVE 'NOME EM BRANCO NAO ACEITA' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 IF FS-STAT = 00
                    MOVE 'NOME JA EXISTE EM OUTRO CODIGO' TO WS-MSGERRO
                    PERFORM MOSTRA-DADOS-VENDEDOR
                    PERFORM MOSTRA-ERRO
                    PERFORM MOSTRA-TELA-REGISTRO
                    DISPLAY SS-CABEC-INCLUSAO
                    DISPLAY(10 32) WS-VD-CODIGO.
       INCLUI-CPF.
           IF WS-OPCAO = 1
              ACCEPT(14 32) WS-VD-CPF WITH PROMPT
              MOVE WS-VD-CPF TO VD-CPF
              READ VENDEDOR KEY IS VD-CHAVE-CPF
              START VENDEDOR KEY IS EQUAL TO VD-CHAVE-CPF
              IF WS-VD-CPF = ZEROS
                 MOVE 0 TO FS-STAT
                 MOVE 'CPF ZERADO NAO ACEITO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 IF FS-STAT = 00 OR FS-STAT = 21
                 MOVE 'CPF EXISTE EM OUTRO VENDEDOR' TO WS-MSGERRO
                 PERFORM MOSTRA-DADOS-VENDEDOR
                 PERFORM MOSTRA-ERRO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-INCLUSAO
                 DISPLAY(10 32) WS-VD-CODIGO
                 DISPLAY(12 32) WS-VD-NOME.
       INCLUI-SEGUE-RESTO.
           IF WS-OPCAO = 1
              ACCEPT(16 32) WS-VD-LATITUDE WITH PROMPT
              MOVE WS-VD-LATITUDE TO WS-MASCARA
              DISPLAY(16 32) WS-MASCARA
              ACCEPT(18 32) WS-VD-LONGITUDE WITH PROMPT
              MOVE WS-VD-LONGITUDE TO WS-MASCARA
              DISPLAY(18 32) WS-MASCARA.
       INCLUI-GRAVAR.
           IF WS-OPCAO = 1
              IF WS-VD-NOME EQUAL SPACES OR WS-VD-CPF  EQUAL 0
                 MOVE 'CAMPOS CPF OU NOME SEM PREENCHIMENTO'
                       TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 MOVE WS-REG-VENDEDOR TO REG-VENDEDOR
                 WRITE REG-VENDEDOR
                 IF FS-STAT = 21
                    CLOSE VENDEDOR OPEN I-O VENDEDOR
                    WRITE REG-VENDEDOR
                 ELSE
                    IF FS-STAT NOT = 00
                       DISPLAY(23 14) 'ERRO ' FS-STAT ' AO GRAVAR'
                       PERFORM TEMPO 10000 TIMES
                       DISPLAY(23 14) '                           '
                       MOVE 0 TO FS-STAT
                       DISPLAY(23 14) 'NAO GRAVOU. ERRO ' FS-STAT.
       INCLUI-FIM.

      * -----------------------------------
       IMPORTAR.
           PERFORM MOSTRA-TELA-REGISTRO
           DISPLAY SS-CABEC-IMPORTAR
           OPEN INPUT VENCSV
           IF WS-OPCAO = 2
              IF FS-STAT NOT = 00
                 MOVE 'ARQUIVO NAO ENCONTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 23 TO FS-STAT.
           IF WS-OPCAO = 2
              READ VENCSV
              PERFORM IMPORTA-CODIGO
                      UNTIL FS-STAT NOT = 00.
       IMPORTA-CODIGO.
           READ VENCSV AT END
              MOVE 23 TO FS-STAT
              CLOSE VENCSV.
           MOVE 1 TO IDX IDX-COD
           MOVE 3 TO CT
           PERFORM PEGA-CODIGO UNTIL VS-LINHA(IDX) = ';'
           IF IDX = 1
              MOVE 23 TO FS-STAT
              CLOSE VENCSV.

           PERFORM MOVE-CODIGO UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 14 TO CT
           PERFORM PEGA-CPF  UNTIL VS-LINHA(IDX) = ';'
           PERFORM MOVE-CPF  UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 40 TO CT
           PERFORM PEGA-NOME  UNTIL VS-LINHA(IDX) = ';'
           PERFORM MOVE-NOME  UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 11 TO CT
           PERFORM PEGA-LA UNTIL VS-LINHA(IDX) = ','
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LA UNTIL VS-LINHA(IDX) = '-'
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LA UNTIL VS-LINHA(IDX) = ';'
           PERFORM MOVE-LA UNTIL IDX > 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 11 TO CT
           PERFORM PEGA-LO UNTIL VS-LINHA(IDX) = ','
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LO UNTIL VS-LINHA(IDX) = '-'
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LO UNTIL VS-LINHA(IDX) = ';'
           PERFORM MOVE-LO UNTIL IDX > 0.
       PEGA-CODIGO.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-CODIGO.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE VS-LINHA(IDX) TO RE-CODIGO(CT)
              SUBTRACT 1 FROM CT.
       PEGA-CPF.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-CPF.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE VS-LINHA(IDX) TO RE-CPF(CT)
              SUBTRACT 1 FROM CT.
       PEGA-NOME.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-NOME.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE VS-LINHA(IDX) TO RE-NOME(CT)
              SUBTRACT 1 FROM CT.
       PEGA-LA.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-LA.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              IF VS-LINHA(IDX) = ','
                 GO MOVE-LA
              ELSE
                 MOVE VS-LINHA(IDX) TO RE-LATITUDE(CT)
                 SUBTRACT 1 FROM CT.
       PEGA-LO.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-LO.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              IF VS-LINHA(IDX) = ','
                 GO MOVE-LO
              ELSE
                 MOVE VS-LINHA(IDX) TO RE-LONGITUDE(CT)
                 SUBTRACT 1 FROM CT.
        IMPORTA-FIM.
      * -----------------------------------
       ALTERA.
           PERFORM MOSTRA-TELA-REGISTRO
           DISPLAY SS-CABEC-ALTERACAO
           IF WS-OPCAO = 3
              PERFORM ALTERA-CODIGO
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 3
              MOVE 0 TO FS-STAT
              PERFORM ALTERA-NOME
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 3
              MOVE 0 TO FS-STAT
              PERFORM ALTERA-CPF
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 3
              PERFORM ALTERA-SEGUE-RESTO THRU ALTERA-GRAVAR
                      UNTIL FS-STAT = 00.
       ALTERA-CODIGO.
           ACCEPT(10 32) WS-VD-CODIGO WITH PROMPT
           IF WS-VD-CODIGO > 0
              MOVE WS-VD-CODIGO TO VD-CODIGO
              READ VENDEDOR
              IF FS-STAT NOT = 00
                 MOVE 'VENDEDOR NAO CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-VD-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-IMPORTAR
              ELSE
                 PERFORM MOSTRA-DADOS-VENDEDOR
                 MOVE REG-VENDEDOR TO WS-REG-VENDEDOR
                 MOVE 3 TO WS-OPCAO
                 MOVE 23 TO FS-STAT
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       ALTERA-NOME.
           MOVE VD-NOME TO XX-VD-NOME
           IF WS-OPCAO = 3
              ACCEPT(12 32) WS-VD-NOME WITH PROMPT UPDATE
              MOVE WS-VD-NOME TO VD-NOME
              READ VENDEDOR KEY IS VD-NOME
              START VENDEDOR KEY IS EQUAL TO VD-NOME
              IF WS-VD-NOME = SPACES
                 MOVE 0 TO FS-STAT
                 MOVE 'NOME EM BRANCO NAO ACEITA' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE WS-VD-CODIGO TO VD-CODIGO
                 READ VENDEDOR
                 PERFORM MOSTRA-DADOS-VENDEDOR
              ELSE
                 IF WS-VD-NOME NOT = XX-VD-NOME
                    IF FS-STAT = 00
                       MOVE 'NOME JA EXISTE EM OUTRO CODIGO' 
                            TO WS-MSGERRO
                       PERFORM MOSTRA-DADOS-VENDEDOR
                       PERFORM MOSTRA-ERRO
                       PERFORM MOSTRA-TELA-REGISTRO
                       MOVE WS-VD-CODIGO TO VD-CODIGO
                       READ VENDEDOR
                       MOVE VD-NOME TO WS-VD-NOME
                       DISPLAY SS-CABEC-ALTERACAO
                       PERFORM MOSTRA-DADOS-VENDEDOR
                    ELSE
                       MOVE 23 TO FS-STAT
                 ELSE
                    MOVE 23 TO FS-STAT.
       ALTERA-CPF.
           MOVE WS-VD-CPF TO XX-VD-CPF
           IF WS-OPCAO = 3
              ACCEPT(14 32) WS-VD-CPF WITH PROMPT UPDATE
              MOVE WS-VD-CPF TO VD-CPF
              READ VENDEDOR KEY IS VD-CHAVE-CPF
              START VENDEDOR KEY IS EQUAL TO VD-CHAVE-CPF
              IF WS-VD-CPF = ZEROS
                 MOVE 0 TO FS-STAT
                 MOVE 'CPF ZERADO NAO ACEITO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE WS-VD-CODIGO TO VD-CODIGO
                 READ VENDEDOR
                 PERFORM MOSTRA-DADOS-VENDEDOR
              ELSE
                 IF WS-VD-CPF NOT = XX-VD-CPF
                    IF FS-STAT = 00 OR FS-STAT = 21
                       MOVE 'CPF EXISTE EM OUTRO VENDEDOR' TO WS-MSGERRO
                       PERFORM MOSTRA-DADOS-VENDEDOR
                       PERFORM MOSTRA-ERRO
                       PERFORM MOSTRA-TELA-REGISTRO
                       MOVE WS-VD-CODIGO TO VD-CODIGO
                       READ VENDEDOR
                       MOVE VD-CPF TO WS-VD-CPF
                       DISPLAY SS-CABEC-ALTERACAO
                       PERFORM MOSTRA-DADOS-VENDEDOR
                    ELSE
                       MOVE 23 TO FS-STAT
                 ELSE
                    MOVE 23 TO FS-STAT.
       ALTERA-SEGUE-RESTO.
           IF WS-OPCAO = 3
              ACCEPT(16 32) WS-VD-LATITUDE WITH PROMPT UPDATE
              MOVE WS-VD-LATITUDE TO WS-MASCARA
              DISPLAY(16 32) WS-MASCARA
              ACCEPT(18 32) WS-VD-LONGITUDE WITH PROMPT UPDATE
              MOVE WS-VD-LONGITUDE TO WS-MASCARA
              DISPLAY(18 32) WS-MASCARA.
       ALTERA-GRAVAR.
           IF WS-OPCAO = 3
              IF WS-VD-NOME EQUAL SPACES OR WS-VD-CPF  EQUAL 0
                 MOVE 'CAMPOS CPF OU NOME SEM PREENCHIMENTO'
                       TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 MOVE WS-REG-VENDEDOR TO REG-VENDEDOR
                 REWRITE REG-VENDEDOR
                 IF FS-STAT = 21
                    CLOSE VENDEDOR OPEN I-O VENDEDOR
                    REWRITE REG-VENDEDOR
                 ELSE
                    IF FS-STAT NOT = 00
                       DISPLAY(23 14) 'ERRO ' FS-STAT ' AO GRAVAR'
                       PERFORM TEMPO 10000 TIMES
                       DISPLAY(23 14) '                           '
                       MOVE 0 TO FS-STAT
                       DISPLAY(23 14) 'NAO GRAVOU. ERRO ' FS-STAT.
       ALTERA-FIM.
      * -----------------------------------
       EXCLUI.
           PERFORM MOSTRA-TELA-REGISTRO
           DISPLAY SS-CABEC-EXCLUSAO.
           IF WS-OPCAO = 4
              PERFORM EXCLUI-CODIGO
                      UNTIL FS-STAT = 23.
       EXCLUI-CODIGO.
           ACCEPT(10 32) WS-VD-CODIGO WITH PROMPT
           IF WS-VD-CODIGO > 0
              MOVE WS-VD-CODIGO TO VD-CODIGO
              READ VENDEDOR
              IF FS-STAT NOT = 00
                 MOVE 'VENDEDOR NAO CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-VD-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-IMPORTAR
                 MOVE 00 TO FS-STAT
              ELSE
                 PERFORM MOSTRA-DADOS-VENDEDOR
                 PERFORM EXCLUI-RESPOSTA
                         UNTIL WS-RESPOSTA = 'S' OR WS-RESPOSTA = 'N'
                 MOVE 4 TO WS-OPCAO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-EXCLUSAO
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       EXCLUI-RESPOSTA.
           DISPLAY(23 14) 'DESEJA EXCLUIR VENDEDOR ? (S/N) '
           ACCEPT(23 45) WS-RESPOSTA
           IF WS-RESPOSTA = 'S'
              DELETE VENDEDOR
              MOVE 'VENDEDOR EXCLUIDO COM SUCESSO !!!!' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO.
           IF WS-RESPOSTA = 'S' OR WS-RESPOSTA = 'N'
              PERFORM MOSTRA-TELA-REGISTRO
              DISPLAY SS-CABEC-EXCLUSAO.
       EXCLUI-FIM.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAIDA
       ABRIR-ARQUIVOS.
           OPEN I-O VENDEDOR
           IF FS-STAT = '30'
               OPEN OUTPUT VENDEDOR
               CLOSE VENDEDOR
               OPEN I-O VENDEDOR.
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DO ARQUIVO: " FS-STAT
               PERFORM TEMPO 5000 TIMES
               DISPLAY(23 14) '                           '.
      * -----------------------------------
      * MOSTRA TELA DO REGISTRO SEM INFORMACOES
       MOSTRA-TELA-REGISTRO.
           DISPLAY SS-JANELA
           DISPLAY SS-TELA-REGISTRO.
      * -----------------------------------
      * MOSTRA MENSAGEM, ESPERA ENTER, ATUALIZA BARRA STATUS
       MOSTRA-ERRO.
           DISPLAY(23 14) WS-MSGERRO
           PERFORM TEMPO 10000 TIMES
           MOVE SPACES TO WS-MSGERRO.
      * -----------------------------------
      * MOSTRA DADOS DO VENDEDOR
       MOSTRA-DADOS-VENDEDOR.
           DISPLAY(10 32) VD-CODIGO
           DISPLAY(12 32) VD-NOME
           DISPLAY(14 32) VD-CPF
           MOVE VD-LATITUDE TO WS-MASCARA
           DISPLAY(16 32) WS-MASCARA
           MOVE VD-LONGITUDE TO WS-MASCARA
           DISPLAY(18 32) WS-MASCARA.
      * -----------------------------------
       FINALIZA.
           DISPLAY(01 01) ERASE
           CLOSE VENDEDOR
           CHAIN "SICCV.EXE"
           STOP RUN.

      * -----------------------------------
       TEMPO.
           DISPLAY(23 13) " ".
