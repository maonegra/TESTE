       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SICCV01.
       AUTHOR.       FRANCISCO
       INSTALLATION. HBSIS.
       DATE-WRITTEN. 11/07/2019.
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
      * OBJETIVO.....: SICCV01 - CRUD DE CLIENTES                           *
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLICSV ASSIGN TO DISK
                  FILE STATUS FS-STAT
                  ORGANIZATION IS LINE SEQUENTIAL.
       COPY CLIENTES.SEL.

       DATA DIVISION.
       FILE SECTION.
       FD  CLICSV   LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "cliente.csv".
       01  REG-CLICSV.
           05 CS-LINHA          PIC X(01) OCCURS 83 TIMES.
       COPY CLIENTES.FD.
       WORKING-STORAGE SECTION.
       01  IDX-COD              PIC 9(02) VALUE ZEROS.
       01  IDX                  PIC 9(02) VALUE ZEROS.
       01  CT                   PIC 9(02) VALUE ZEROS.
       01  FS-STAT              PIC 9(02) VALUE ZEROS.
       01  WS-OPCAO             PIC 9(01) VALUE ZEROS.
       01  WS-MSGERRO           PIC X(43).
       01  WS-MASCARA           PIC ZZ9,99999999 VALUE ZEROS.
       01  WS-RESPOSTA          PIC X(01) VALUE SPACES.
       01  XX-CL-RAZAO          PIC X(40).
       01  XX-CL-CNPJ           PIC 9(14).
       01  WS-REG-CLIENTES.
           05 WS-CL-CODIGO      PIC 9(07).
           05 WS-CODIGO REDEFINES WS-CL-CODIGO.
              10 RE-CODIGO      PIC 9 OCCURS 7 TIMES.
           05 WS-CL-RAZAO       PIC X(40).
           05 WS-RAZAO  REDEFINES WS-CL-RAZAO.
              10 RE-RAZAO       PIC X OCCURS 40 TIMES.
           05 WS-CL-CHAVE-CNPJ.
              10 WS-CL-CNPJ     PIC 9(14).
              10 WS-CNPJ REDEFINES WS-CL-CNPJ.
                 10 RE-CNPJ     PIC 9 OCCURS 14 TIMES.
           05 WS-CL-LATITUDE    PIC S9(03)V9(8).
           05 WS-LATITUDE REDEFINES WS-CL-LATITUDE.
              10 RE-LATITUDE    PIC 9 OCCURS 11 TIMES.
           05 WS-CL-LONGITUDE   PIC S9(03)V9(8).
           05 WS-LONGITUDE REDEFINES WS-CL-LONGITUDE.
              10 RE-LONGITUDE   PIC 9 OCCURS 11 TIMES.
           05 WS-CL-VENDEDOR       PIC 9(03) VALUE ZEROS.

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
               "*** CADASTRO DE CLIENTES ***".
       01  SS-CABEC-INCLUSAO.
           02 LINE 04 COLUMN 21 VALUE
              "I N C L U S A O    D E   C L I E N T E S  ".
       01  SS-CABEC-IMPORTAR.
           02 LINE 04 COLUMN 21 VALUE
              "   I M P O R T A R   O S   C L I E N T E S".
       01  SS-CABEC-ALTERACAO.
           02 LINE 04 COLUMN 21 VALUE
              "A L T E R A C A O   D E   C L I E N T E S ".
       01  SS-CABEC-EXCLUSAO.
           02 LINE 04 COLUMN 21 VALUE
              "E X C L U S A O    D E   C L I E N T E S  ".
       01  SS-MENU.
           02 LINE 07 COLUMN 33         VALUE "1 - INCLUIR".
           02 LINE 08 COLUMN 33         VALUE "2 - IMPORTAR".
           02 LINE 09 COLUMN 33         VALUE "3 - ALTERAR".
           02 LINE 10 COLUMN 33         VALUE "4 - EXCLUIR".
           02 LINE 11 COLUMN 33         VALUE "9 - ENCERRAR".
           02 LINE 13 COLUMN 33         VALUE "OPCAO: ".
           02  ss-opcao LINE 13 COLUMN 40 PIC 9 USING WS-OPCAO AUTO.
       01  SS-TELA-REGISTRO.
           02 LINE 10 COLUMN 12 VALUE "C๓digo do Cliente.:
      -       "                           ".
           02 LINE 12 COLUMN 12 VALUE "Razao Social......:
      -       "                           ".
           02 LINE 14 COLUMN 12 VALUE "C.N.P.J...........:
      -       "                           ".
           02 LINE 16 COLUMN 12 VALUE "Latitude..........:
      -       "                           ".
           02 LINE 18 COLUMN 12 VALUE "Longitude.........:
      -       "                           ".
       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO WS-OPCAO
           PERFORM ABRIR-ARQUIVOS
           PERFORM PROCESSA THRU SEGUE-01 UNTIL WS-OPCAO = 9.
      * -----------------------------------
       FINALIZA.
           DISPLAY(01 01) ERASE
           CLOSE CLIENTES
           CHAIN "SICCV.EXE"
           STOP RUN.
       PROCESSA.
           DISPLAY SS-JANELA
           DISPLAY SS-MENU
           ACCEPT SS-MENU.
       SEGUE-01.
           MOVE 1     TO WS-CL-CODIGO
           MOVE ZEROS TO WS-CL-CNPJ
                         WS-CL-LATITUDE WS-CL-LONGITUDE
           MOVE SPACES TO WS-CL-RAZAO.
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
              PERFORM INCLUI-RAZAO
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 1
              MOVE 0 TO FS-STAT
              PERFORM INCLUI-CNPJ
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 1
              PERFORM INCLUI-SEGUE-RESTO THRU INCLUI-GRAVAR
                      UNTIL FS-STAT = 00.
       INCLUI-CODIGO.
           ACCEPT(10 32) WS-CL-CODIGO WITH PROMPT
           IF WS-CL-CODIGO > 0
              MOVE WS-CL-CODIGO TO CL-CODIGO
              READ CLIENTES
              IF FS-STAT = 00
                 PERFORM MOSTRA-DADOS-CLIENTE
                 MOVE 'CLIENTE JA CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-CL-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-INCLUSAO
              ELSE
                 MOVE 1 TO WS-OPCAO
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       INCLUI-RAZAO.
           IF WS-OPCAO = 1
              ACCEPT(12 32) WS-CL-RAZAO WITH PROMPT
              MOVE WS-CL-RAZAO TO CL-RAZAO
              READ CLIENTES KEY IS CL-RAZAO
              START CLIENTES KEY IS EQUAL TO CL-RAZAO
              IF WS-CL-RAZAO = SPACES
                 MOVE 0 TO FS-STAT
                 MOVE 'RAZAO EM BRANCO NAO ACEITA' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 IF FS-STAT = 00
                    MOVE 'RAZAO JA EXISTE EM OUTRO CODIGO' TO WS-MSGERRO
                    PERFORM MOSTRA-DADOS-CLIENTE
                    PERFORM MOSTRA-ERRO
                    PERFORM MOSTRA-TELA-REGISTRO
                    DISPLAY SS-CABEC-INCLUSAO
                    DISPLAY(10 32) WS-CL-CODIGO.
       INCLUI-CNPJ.
           IF WS-OPCAO = 1
              ACCEPT(14 32) WS-CL-CNPJ WITH PROMPT
              MOVE WS-CL-CNPJ TO CL-CNPJ
              READ CLIENTES KEY IS CL-CHAVE-CNPJ
              START CLIENTES KEY IS EQUAL TO CL-CHAVE-CNPJ
              IF WS-CL-CNPJ = ZEROS
                 MOVE 0 TO FS-STAT
                 MOVE 'CNPJ ZERADO NAO ACEITO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 IF FS-STAT = 00 OR FS-STAT = 21
                 MOVE 'CNPJ EXISTE EM OUTRO CLIENTE' TO WS-MSGERRO
                 PERFORM MOSTRA-DADOS-CLIENTE
                 PERFORM MOSTRA-ERRO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-INCLUSAO
                 DISPLAY(10 32) WS-CL-CODIGO
                 DISPLAY(12 32) WS-CL-RAZAO.
       INCLUI-SEGUE-RESTO.
           IF WS-OPCAO = 1
              ACCEPT(16 32) WS-CL-LATITUDE WITH PROMPT
              MOVE WS-CL-LATITUDE TO WS-MASCARA
              DISPLAY(16 32) WS-MASCARA
              ACCEPT(18 32) WS-CL-LONGITUDE WITH PROMPT
              MOVE WS-CL-LONGITUDE TO WS-MASCARA
              DISPLAY(18 32) WS-MASCARA.
       INCLUI-GRAVAR.
           IF WS-OPCAO = 1
              IF WS-CL-RAZAO EQUAL SPACES OR WS-CL-CNPJ  EQUAL 0
                 MOVE 'CAMPOS CNPJ OU RAZAO SEM PREENCHIMENTO'
                       TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 MOVE WS-REG-CLIENTES TO REG-CLIENTES
                 WRITE REG-CLIENTES
                 IF FS-STAT = 21
                    CLOSE CLIENTES OPEN I-O CLIENTES
                    WRITE REG-CLIENTES
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
           OPEN INPUT CLICSV
           IF WS-OPCAO = 2
              IF FS-STAT NOT = 00
                 MOVE 'ARQUIVO NAO ENCONTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 23 TO FS-STAT.
           IF WS-OPCAO = 2
              READ CLICSV
              PERFORM IMPORTA-CODIGO
                      UNTIL FS-STAT NOT = 00.
       IMPORTA-CODIGO.
           READ CLICSV AT END
              MOVE 23 TO FS-STAT
              CLOSE CLICSV.
           MOVE 1 TO IDX IDX-COD
           MOVE 7 TO CT
           PERFORM PEGA-CODIGO UNTIL CS-LINHA(IDX) = ';'
           IF IDX = 1
              MOVE 23 TO FS-STAT
              CLOSE CLICSV.

           PERFORM MOVE-CODIGO UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 14 TO CT
           PERFORM PEGA-CNPJ UNTIL CS-LINHA(IDX) = ';'
           PERFORM MOVE-CNPJ UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 40 TO CT
           PERFORM PEGA-RAZAO UNTIL CS-LINHA(IDX) = ';'
           PERFORM MOVE-RAZAO UNTIL IDX = 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 11 TO CT
           PERFORM PEGA-LA UNTIL CS-LINHA(IDX) = ','
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LA UNTIL CS-LINHA(IDX) = '-'
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LA UNTIL CS-LINHA(IDX) = ';'
           PERFORM MOVE-LA UNTIL IDX > 0

           ADD 1 TO IDX-COD
           MOVE IDX-COD TO IDX
           MOVE 11 TO CT
           PERFORM PEGA-LO UNTIL CS-LINHA(IDX) = ','
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LO UNTIL CS-LINHA(IDX) = '-'
           SUBTRACT 1 FROM IDX
           PERFORM PEGA-LO UNTIL CS-LINHA(IDX) = ';'
           PERFORM MOVE-LO UNTIL IDX > 0.
       PEGA-CODIGO.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-CODIGO.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE CS-LINHA(IDX) TO RE-CODIGO(CT)
              SUBTRACT 1 FROM CT.
       PEGA-CNPJ.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-CNPJ.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE CS-LINHA(IDX) TO RE-CNPJ(CT)
              SUBTRACT 1 FROM CT.
       PEGA-RAZAO.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-RAZAO.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              MOVE CS-LINHA(IDX) TO RE-RAZAO(CT)
              SUBTRACT 1 FROM CT.
       PEGA-LA.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-LA.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              IF CS-LINHA(IDX) = ','
                 GO MOVE-LA
              ELSE
                 MOVE CS-LINHA(IDX) TO RE-LATITUDE(CT)
                 SUBTRACT 1 FROM CT.
       PEGA-LO.
           ADD 1 TO IDX
           ADD 1 TO IDX-COD.
       MOVE-LO.
           SUBTRACT 1 FROM IDX
           IF IDX NOT = 0
              IF CS-LINHA(IDX) = ','
                 GO MOVE-LO
              ELSE
                 MOVE CS-LINHA(IDX) TO RE-LONGITUDE(CT)
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
              PERFORM ALTERA-RAZAO
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 3
              MOVE 0 TO FS-STAT
              PERFORM ALTERA-CNPJ
                      UNTIL FS-STAT = 23.
           IF WS-OPCAO = 3
              PERFORM ALTERA-SEGUE-RESTO THRU ALTERA-GRAVAR
                      UNTIL FS-STAT = 00.
       ALTERA-CODIGO.
           ACCEPT(10 32) WS-CL-CODIGO WITH PROMPT
           IF WS-CL-CODIGO > 0
              MOVE WS-CL-CODIGO TO CL-CODIGO
              READ CLIENTES
              IF FS-STAT NOT = 00
                 MOVE 'CLIENTE NAO CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-CL-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-IMPORTAR
              ELSE
                 PERFORM MOSTRA-DADOS-CLIENTE
                 MOVE REG-CLIENTES TO WS-REG-CLIENTES
                 MOVE 3 TO WS-OPCAO
                 MOVE 23 TO FS-STAT
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       ALTERA-RAZAO.
           MOVE CL-RAZAO TO XX-CL-RAZAO
           IF WS-OPCAO = 3
              ACCEPT(12 32) WS-CL-RAZAO WITH PROMPT UPDATE
              MOVE WS-CL-RAZAO TO CL-RAZAO
              READ CLIENTES KEY IS CL-RAZAO
              START CLIENTES KEY IS EQUAL TO CL-RAZAO
              IF WS-CL-RAZAO = SPACES
                 MOVE 0 TO FS-STAT
                 MOVE 'RAZAO EM BRANCO NAO ACEITA' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE WS-CL-CODIGO TO CL-CODIGO
                 READ CLIENTES
                 PERFORM MOSTRA-DADOS-CLIENTE
              ELSE
                 IF WS-CL-RAZAO NOT = XX-CL-RAZAO
                    IF FS-STAT = 00
                       MOVE 'RAZAO JA EXISTE EM OUTRO CODIGO'
                            TO WS-MSGERRO
                       PERFORM MOSTRA-DADOS-CLIENTE
                       PERFORM MOSTRA-ERRO
                       PERFORM MOSTRA-TELA-REGISTRO
                       MOVE WS-CL-CODIGO TO CL-CODIGO
                       READ CLIENTES
                       MOVE CL-RAZAO TO WS-CL-RAZAO
                       DISPLAY SS-CABEC-ALTERACAO
                       PERFORM MOSTRA-DADOS-CLIENTE
                    ELSE
                       MOVE 23 TO FS-STAT
                 ELSE
                    MOVE 23 TO FS-STAT.
       ALTERA-CNPJ.
           MOVE WS-CL-CNPJ TO XX-CL-CNPJ
           IF WS-OPCAO = 3
              ACCEPT(14 32) WS-CL-CNPJ WITH PROMPT UPDATE
              MOVE WS-CL-CNPJ TO CL-CNPJ
              READ CLIENTES KEY IS CL-CHAVE-CNPJ
              START CLIENTES KEY IS EQUAL TO CL-CHAVE-CNPJ
              IF WS-CL-CNPJ = ZEROS
                 MOVE 0 TO FS-STAT
                 MOVE 'CNPJ ZERADO NAO ACEITO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE WS-CL-CODIGO TO CL-CODIGO
                 READ CLIENTES
                 PERFORM MOSTRA-DADOS-CLIENTE
              ELSE
                 IF WS-CL-CNPJ NOT = XX-CL-CNPJ
                    IF FS-STAT = 00 OR FS-STAT = 21
                       MOVE 'CNPJ EXISTE EM OUTRO CLIENTE' TO WS-MSGERRO
                       PERFORM MOSTRA-DADOS-CLIENTE
                       PERFORM MOSTRA-ERRO
                       PERFORM MOSTRA-TELA-REGISTRO
                       MOVE WS-CL-CODIGO TO CL-CODIGO
                       READ CLIENTES
                       MOVE CL-CNPJ TO WS-CL-CNPJ
                       DISPLAY SS-CABEC-ALTERACAO
                       PERFORM MOSTRA-DADOS-CLIENTE
                    ELSE
                       MOVE 23 TO FS-STAT
                 ELSE
                    MOVE 23 TO FS-STAT.
       ALTERA-SEGUE-RESTO.
           IF WS-OPCAO = 3
              ACCEPT(16 32) WS-CL-LATITUDE WITH PROMPT UPDATE
              MOVE WS-CL-LATITUDE TO WS-MASCARA
              DISPLAY(16 32) WS-MASCARA
              ACCEPT(18 32) WS-CL-LONGITUDE WITH PROMPT UPDATE
              MOVE WS-CL-LONGITUDE TO WS-MASCARA
              DISPLAY(18 32) WS-MASCARA.
       ALTERA-GRAVAR.
           IF WS-OPCAO = 3
              IF WS-CL-RAZAO EQUAL SPACES OR WS-CL-CNPJ  EQUAL 0
                 MOVE 'CAMPOS CNPJ OU RAZAO SEM PREENCHIMENTO'
                       TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
              ELSE
                 MOVE WS-REG-CLIENTES TO REG-CLIENTES
                 REWRITE REG-CLIENTES
                 IF FS-STAT = 21
                    CLOSE CLIENTES OPEN I-O CLIENTES
                    REWRITE REG-CLIENTES
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
           ACCEPT(10 32) WS-CL-CODIGO WITH PROMPT
           IF WS-CL-CODIGO > 0
              MOVE WS-CL-CODIGO TO CL-CODIGO
              READ CLIENTES
              IF FS-STAT NOT = 00
                 MOVE 'CLIENTE NAO CADASTRADO' TO WS-MSGERRO
                 PERFORM MOSTRA-ERRO
                 MOVE 0 TO WS-CL-CODIGO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-IMPORTAR
                 MOVE 00 TO FS-STAT
              ELSE
                 PERFORM MOSTRA-DADOS-CLIENTE
                 PERFORM EXCLUI-RESPOSTA
                         UNTIL WS-RESPOSTA = 'S' OR WS-RESPOSTA = 'N'
                 MOVE 4 TO WS-OPCAO
                 PERFORM MOSTRA-TELA-REGISTRO
                 DISPLAY SS-CABEC-EXCLUSAO
           ELSE
              MOVE 0 TO WS-OPCAO
              MOVE 23 TO FS-STAT.
       EXCLUI-RESPOSTA.
           DISPLAY(23 14) 'DESEJA EXCLUIR CLIENTE ? (S/N) '
           ACCEPT(23 45) WS-RESPOSTA
           IF WS-RESPOSTA = 'S'
              DELETE CLIENTES
              MOVE 'CLIENTE EXCLUIDO COM SUCESSO !!!!' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO.
           IF WS-RESPOSTA = 'S' OR WS-RESPOSTA = 'N'
              PERFORM MOSTRA-TELA-REGISTRO
              DISPLAY SS-CABEC-EXCLUSAO.
       EXCLUI-FIM.
      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAIDA
       ABRIR-ARQUIVOS.
           OPEN I-O CLIENTES
           IF FS-STAT = '30'
               OPEN OUTPUT CLIENTES
               CLOSE CLIENTES
               OPEN I-O CLIENTES.
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
      * MOSTRA DADOS DO CLIENTE
       MOSTRA-DADOS-CLIENTE.
           DISPLAY(10 32) CL-CODIGO
           DISPLAY(12 32) CL-RAZAO
           DISPLAY(14 32) CL-CNPJ
           MOVE CL-LATITUDE TO WS-MASCARA
           DISPLAY(16 32) WS-MASCARA
           MOVE CL-LONGITUDE TO WS-MASCARA
           DISPLAY(18 32) WS-MASCARA.

      * -----------------------------------
       TEMPO.
           DISPLAY(23 13) " ".
