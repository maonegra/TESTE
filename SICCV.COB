       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SICCV.
       AUTHOR.       FRANCISCO
       INSTALLATION. HBSIS.
       DATE-WRITTEN. 10/07/2019
       DATE-COMPILED.
       SECURITY.
      *
      ***********************************************************************
      * SISTEMA......: SICCV – SISTEMA DE CONTROLE DE CLIENTES POR VENDEDOR *
      ***********************************************************************
      * ANALISTA.....: FRANCISCO A. ROLIM DE MOURA JR                       *
      * LINGUAGEM....: COBOL                                                *
      * PROGRAMADOR..: FRANCISCO A. ROLIM DE MOURA JR                       *
      * DATA.........: 10/07/2019                                           *
      ***********************************************************************
      * OBJETIVO.....: MENU PRINCIPAL DO SICCV                              *
      ***********************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-OPCAO      PIC 9 VALUE ZERO.

       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  OPCOES-MENU.
           02  LINE 01 COLUMN 18 VALUE "SISTEMA DE CONTROLE DE CLIENTES
      -        "POR VENDEDOR".
           02  LINE 03 COLUMN 28  VALUE "     C A D A S T R O S     ".
           02  LINE 04 COLUMN 28  VALUE "===========================".
           02  LINE 05 COLUMN 28  VALUE "1. Cadastro de Clientes    ".
           02  LINE 06 COLUMN 28  VALUE "2. Cadastro de Vendedores  ".
           02  LINE 08 COLUMN 28  VALUE "    R E L A T O R I O S    ".
           02  LINE 09 COLUMN 28  VALUE "===========================".
           02  LINE 10 COLUMN 28  VALUE "3. Cadastro de Clientes    ".
           02  LINE 11 COLUMN 28  VALUE "4. Cadastro de Vendedores  ".
           02  LINE 13 COLUMN 28  VALUE "      E X E C U T A R      ".
           02  LINE 14 COLUMN 28  VALUE "===========================".
           02  LINE 15 COLUMN 28  VALUE "5. Distribuicao de clientes".
           02  LINE 17 COLUMN 28  VALUE "9. SAIR DO SISTEMA         ".
           02  LINE 22 COLUMN 01  VALUE "Mensagem: Digite sua Opcao: ".
           02  ss-opcao LINE 22 COLUMN 28 PIC 9 USING WS-OPCAO AUTO.
       PROCEDURE DIVISION.
       MOSTRAR-MENU.
           DISPLAY LIMPA-TELA
           DISPLAY OPCOES-MENU
           PERFORM TESTA-OPCAO UNTIL WS-OPCAO = 9
           DISPLAY LIMPA-TELA
           STOP "fim do programa"
           STOP RUN.
       TESTA-OPCAO.
           ACCEPT OPCOES-MENU.
           IF WS-OPCAO = 1
              DISPLAY(22 30) "Acessando Cadastro de Clientes..."
              MOVE 0 TO WS-OPCAO
              CHAIN "SICCV01.EXE".
           IF WS-OPCAO = 2
              DISPLAY(22 30) "Acessando Cadastro de Vendedores..."
              MOVE 0 TO WS-OPCAO
              CHAIN "SICCV02.EXE".
           IF WS-OPCAO = 3
              DISPLAY(22 30) "Acessando Relatorio de Clientes..."
              MOVE 0 TO WS-OPCAO
              CHAIN "SICCV03.EXE".
           IF WS-OPCAO = 4
              DISPLAY(22 30) "Acessando Relatorio de Vendedores..."
              MOVE 0 TO WS-OPCAO
              CHAIN "SICCV04.EXE".
           IF WS-OPCAO = 5
              DISPLAY(22 30) "Acessando Distribuicao de Clientes..."
              MOVE 0 TO WS-OPCAO
              CHAIN "SICCV05.EXE".

