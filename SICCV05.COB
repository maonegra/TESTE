       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SICCV01.
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
      * DATA.........: 14/07/2019                                           *
      ***********************************************************************
      * OBJETIVO.....: SICCV05 - DISTRIBUICAO DE CLIENTES                   *
      ***********************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY VENDEDOR.SEL.
       COPY CLIENTES.SEL.
           SELECT GERDIST ASSIGN TO DISK
                  FILE STATUS IS FS-STAT
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY GD-CHAVE.
           SELECT CARTEIRA  ASSIGN TO DISK
                  FILE STATUS IS FS-STAT
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY CA-CHAVE.
           SELECT ARQCSV ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       COPY VENDEDOR.SEL.
       COPY CLIENTES.SEL.
       FD  ARQCSV   LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CARTEIRA.CSV".
       01  REG-ARQCSV.
           05 CS-CLIENTE        PIC 9(07).
           05 CS-f1             PIC X(01).
           05 CS-RAZAO          PIC X(40).
           05 CS-f2             PIC X(01).
           05 CS-VENDEDOR       PIC 9(03).
           05 CS-f3             PIC X(01).
           05 CS-NOME           PIC X(40).
           05 CS-f4             PIC X(01).
           05 CS-DISTANCIA      PIC 9(05)V9(3).
       FD  GERDIST LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "gerdist.dat".
       01  REG-GERDIST.
           05 GD-CHAVE.
               10 GD-CLIENTE    PIC 9(07).
               10 GD-DISTANCIA  PIC 9(05)V9(3).
               10 GD-VENDEDOR   PIC 9(3).
           05 GD-FILLER         PIC X.
       FD  CARTEIRA LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "carteira.dat".
       01  REG-CARTEIRA.
           05 CA-CHAVE.
               10 CA-CLIENTE    PIC 9(07).
           05 CA-DISTANCIA      PIC 9(05)V9(3).
           05 CA-VENDEDOR       PIC 9(3).
       WORKING-STORAGE SECTION.
       01  DADOS-LINKAGE.
           05  C-LA                   PIC S9(03)V9(8).
           05  C-LO                   PIC S9(03)V9(8).
           05  V-LA                   PIC S9(03)V9(8).
           05  V-LO                   PIC S9(03)V9(8).
           05  DISTANCIA              PIC  9(05)V9(3).
       01  FS-STAT              PIC 9(02) VALUE ZEROS.
       01  CLIENTE-LIDO         PIC 9(07) VALUE ZEROS.
       01  WS-OPCAO             PIC 9(01) VALUE ZEROS.
       01  WS-MSGERRO           PIC X(43).
       01  WS-REG-CLIENTES.
           05 WS-CL-CODIGO      PIC 9(07).
           05 WS-CL-RAZAO       PIC X(40).
           05 WS-CL-CHAVE-CNPJ.
              10 WS-CL-CNPJ     PIC 9(14).
           05 WS-CL-LATITUDE    PIC S9(03)V9(8).
           05 WS-CL-LONGITUDE   PIC S9(03)V9(8).
           05 WS-CL-VENDEDOR    PIC 9(03).
       01  WS-REG-VENDEDOR.
           05 WS-VD-CODIGO      PIC 9(03).
           05 WS-VD-NOME       PIC X(40).
           05 WS-VD-CHAVE-CPF.
              10 WS-VD-CPF     PIC 9(14).
           05 WS-VD-LATITUDE    PIC S9(03)V9(8).
           05 WS-VD-LONGITUDE   PIC S9(03)V9(8).
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
               "*** DISTRIBUI   CLIENTES ***".
       01  SS-CABEC-INCLUSAO.
           02 LINE 04 COLUMN 21 VALUE
              "D I S T R I B U I C A O  D E  C L I E N T E S  ".
       01  SS-MENU.
           02 LINE 09 COLUMN 33         VALUE "1 - DISTRIBUIR".
           02 LINE 11 COLUMN 33         VALUE "9 - ENCERRAR".
           02 LINE 13 COLUMN 33         VALUE "OPCAO: ".
           02  ss-opcao LINE 13 COLUMN 40 PIC 9 USING WS-OPCAO AUTO.
       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO WS-OPCAO
           PERFORM ABRIR-ARQUIVOS
           PERFORM PROCESSA THRU SEGUE-01 UNTIL WS-OPCAO = 9.
      * -----------------------------------
       FINALIZA.
           DISPLAY(01 01) ERASE
           CLOSE CLIENTES  GERDIST CARTEIRA
           CHAIN "SICCV.EXE"
           STOP RUN.
       PROCESSA.
           DISPLAY SS-JANELA
           DISPLAY SS-MENU
           ACCEPT SS-MENU.
       SEGUE-01.
           IF WS-OPCAO = 1
              PERFORM GERAR  UNTIL WS-OPCAO = 0
              CLOSE CLIENTES OPEN I-O CLIENTES
              PERFORM GERA-CARTEIRA UNTIL WS-OPCAO = 7
              OPEN OUTPUT ARQCSV
              perform gera-arqcsv until ws-opcao = 9.
           IF WS-OPCAO > 1 AND WS-OPCAO < 9
              DISPLAY(23 14) "Opcao Invalida !!!"
              PERFORM TEMPO 10000 TIMES
              DISPLAY(23 14) "                  "
              MOVE 0 TO WS-OPCAO.
           MOVE 0 TO FS-STAT.
        GERAR.
           READ VENDEDOR NEXT AT END
           IF FS-STAT NOT = 00
              CLOSE VENDEDOR CLIENTES GERDIST
              OPEN INPUT VENDEDOR CLIENTES
              OPEN I-O GERDIST
              MOVE 0 TO WS-OPCAO
           ELSE
              PERFORM LER-CLIENTE UNTIL WS-OPCAO = 8
              MOVE 1 TO WS-OPCAO.
        LER-CLIENTE.
           READ CLIENTES NEXT AT END
           IF FS-STAT NOT = 00
              CLOSE CLIENTES OPEN I-O CLIENTES
              MOVE 8 TO WS-OPCAO
           ELSE
              MOVE CL-CODIGO TO GD-CLIENTE
              MOVE VD-CODIGO TO GD-VENDEDOR
              MOVE CL-LATITUDE  TO C-LA
              MOVE CL-LONGITUDE TO C-LO
              MOVE VD-LATITUDE  TO V-LA
              MOVE VD-LONGITUDE TO V-LO
              MOVE 0 TO DISTANCIA
              CALL 'FORMULA' USING C-LA C-LO V-LA V-LO DISTANCIA
              MOVE DISTANCIA TO GD-DISTANCIA
              MOVE ' ' TO GD-FILLER
              WRITE REG-GERDIST.
       GERA-CARTEIRA.
           READ GERDIST NEXT AT END
           IF FS-STAT NOT = 0
              MOVE 7 TO WS-OPCAO
              CLOSE GERDIST OPEN I-O GERDIST
              CLOSE CARTEIRA OPEN I-O CARTEIRA
           ELSE
              IF CLIENTE-LIDO = 0
                 MOVE GD-CLIENTE TO CLIENTE-LIDO
              ELSE
                 IF CLIENTE-LIDO NOT = CA-CLIENTE
                    MOVE GD-CLIENTE TO CLIENTE-LIDO CA-CLIENTE 
                    CL-CODIGO
                    MOVE GD-DISTANCIA TO CA-DISTANCIA
                    MOVE GD-VENDEDOR TO CA-VENDEDOR
                    WRITE REG-CARTEIRA
                    READ CLIENTES
                    MOVE GD-VENDEDOR TO CL-VENDEDOR
                    REWRITE REG-CLIENTES.
       GERA-ARQCSV.
           READ CARTEIRA NEXT AT END
           IF FS-STAT NOT = 0
              MOVE 9 TO WS-OPCAO
              CLOSE GERDIST OPEN I-O GERDIST
              CLOSE CARTEIRA OPEN I-O CARTEIRA
              CLOSE ARQCSV
           ELSE
              MOVE CA-CLIENTE TO CL-CODIGO
              READ CLIENTES
              MOVE CA-VENDEDOR TO VD-CODIGO
              READ VENDEDOR
              MOVE CL-CODIGO TO CS-CLIENTE
              MOVE CL-RAZAO TO CS-RAZAO
              MOVE VD-CODIGO TO CS-VENDEDOR
              MOVE VD-NOME TO CS-NOME
              MOVE CA-DISTANCIA TO CS-DISTANCIA
              MOVE ';' TO CS-F1 CS-F2 CS-F3 CS-F4
              WRITE REG-ARQCSV.

      * -----------------------------------
      * ABRE ARQUIVOS
       ABRIR-ARQUIVOS.
           OPEN INPUT CLIENTES
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DOS CLIENTES" FS-STAT
               PERFORM TEMPO 5000 TIMES
               DISPLAY(23 14) '                           '.
           OPEN INPUT VENDEDOR
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DO VENDEDOR" FS-STAT
               PERFORM TEMPO 5000 TIMES
               DISPLAY(23 14) '                           '.
           OPEN I-O GERDIST
           IF FS-STAT = '30'
               OPEN OUTPUT GERDIST
               CLOSE GERDIST
               OPEN I-O GERDIST.
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DO GERDIST: " FS-STAT
               PERFORM TEMPO 5000 TIMES
               DISPLAY(23 14) '                           '.
           OPEN I-O CARTEIRA
           IF FS-STAT = '30'
               OPEN OUTPUT CARTEIRA
               CLOSE CARTEIRA
               OPEN I-O CARTEIRA.
           IF FS-STAT NOT = '00'
               DISPLAY(23 14) "ESTADO DO CARTEIRA " FS-STAT
               PERFORM TEMPO 5000 TIMES
               DISPLAY(23 14) '                           '.
      * -----------------------------------
       TEMPO.
           DISPLAY(23 13) " ".
