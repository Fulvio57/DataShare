Device = 16F1827
Xtal = 8
On_Hardware_Interrupt GoTo Isr

Config1 FOSC_INTOSC, WDTE_OFF, PWRTE_ON, MCLRE_OFF, CP_ON, CPD_ON, BOREN_ON, _
CLKOUTEN_OFF, IESO_OFF, FCMEN_OFF
Config2 WRT_OFF, PLLEN_OFF, STVREN_OFF,LVP_OFF , BORV_19

Declare Serial_Data 8
Declare Hserin_Pin PORTB.1
Declare Hserout_Pin PORTB.2
Declare Hserial_Baud 9600
Declare Hserial_RCSTA = %10010000
Declare Hserial_Clear = On
Declare HSerial_Terminator = CR

Symbol GIE = INTCON.7
Symbol PEIE = INTCON.6
Symbol RCIE = PIE1.5
Symbol RCIF = PIR1.5
Symbol BUFLEN = 64
Symbol RX_pic  = PORTB.1
Symbol TX_pic  = PORTB.2
Symbol RST_SIM = PORTB.3
Symbol led_V = PORTB.4
Symbol led_R = PORTB.3
Symbol apici = $22
Symbol CR = 13
Symbol LF = 10

Dim rxbuffer[BUFLEN] As Byte
Dim numero_1 [13] As Byte
Dim numero_2 [13] As Byte
Dim numero_3 [13] As Byte
Dim numero_4 [13] As Byte
Dim numero_5 [13] As Byte
Dim n_nuber As Byte
Dim cont As Byte
Dim i As Byte
Dim iy As Byte
Dim cnt As Byte
Dim pos[2] As Byte
Dim length As Byte
Dim num_whitelist As Byte
Dim bufpntr As Byte
Dim rxchar, buflast, car_1, stage As Byte
Dim flag As Byte
Dim number_found As Byte
Dim bufready As flag.1
Dim no_number As flag.3

OSCCON = %01110010
OPTION_REG = %00000111
LATA  = %00000000
LATB  = %00000000
TRISA = %00100000
TRISB = %11111011
WPUA = %00100000
CM1CON0.7 = 0
CM2CON0.7 = 0
SRCON0.7 = 0
FVRCON = %00000000
Clear ANSELA
Clear ANSELB
APFCON0 = 0
APFCON1 = 0
PORTA = 0
TRISA = %00100000
TRISB = %00000010
INTCON = %11000000

PIE2 = 0
PIR1 = 0
PIR2 = 0
Clear

main:
num_whitelist = 1
High led_V : High led_R
inizio:
HSerIn 1000,inizio,[Wait("SMS")]
DelayMS 20
test:
HSerOutLn ["AT"]
HSerIn 500,test,[Wait("OK")]
setsim800()
finder_num()

Do
    High led_V
    DelayMS 200
    Low led_V
    DelayMS 200
Loop
Proc setsim800()
inizio1:
    DelayMS 20
    HSerOutLn["AT+IPR=9600"]
    HSerIn 500,inizio1,[Wait(13,10,"OK",13,10)]
    DelayMS 20
    HSerOutLn["AT+CPBS=",34,"SM",34]
    HSerIn 500,inizio1,[Wait("OK")]
    DelayMS 20
    HSerOutLn["ATE0&W"]
    HSerIn 500,inizio1,[Wait("OK")]
    DelayMS 20
EndProc

Proc finder_num()
    Clear no_number
    Clear cnt
    For i = 1 To 5
imp_numeri:
      Clear rxbuffer                ' prepara per inizio ricezione
      Clear stage                   ' nessuno stadio pendente
      Clear bufpntr                 ' inizio  buffer
      HSerOutLn ["AT+CPBR=",Dec i,44,Dec i]
      Set RCIE                      ' inizio carico buffer
      If bufready = 0 Then imp_numeri
      iy = 7                        ' posizione dopo +CPBR=
      Repeat                        ' preferisco ripetizione
        If rxbuffer[iy] = apici Then
            pos[cnt] = iy
            Inc cnt
        EndIf
        Inc iy                      ' aumenta la posizione
      Until iy <= buflast           ' fino al punto dove ha caricato il buffer
      If cnt <> 0 Then              ' trovato una corrispondenza
        SetBit(number_found,i)      ' flag di 5 bit sul risultato ottenuto
        For iy = pos[0]+1 To pos[1]-1 ' dal primo apice
          Select i
            Case 1                  ' se i sta a 1
                numero_1[iy] = rxbuffer[iy]
            Case 2                  ' se i sta a 2
                numero_2[iy] = rxbuffer[iy]
            Case 3                  ' se i sta a 3
                numero_3[iy] = rxbuffer[iy]
            Case 4                  ' se i sta a 4
                numero_4[iy] = rxbuffer[iy]
            Case 5                  ' se i sta a 5
                numero_5[iy] = rxbuffer[iy]
          End Select
        Next iy
      End If
      Clear cnt                     ' completo e rinnova
      High led_R
    Next i
    DelayMS 300
    HRSOutLn Str numero_1
    DelayMS 50
    HRSOutLn Str numero_2
    DelayMS 50
    HRSOutLn Str numero_3
    DelayMS 50
    HRSOutLn Str numero_4
    DelayMS 50
    HRSOutLn Str numero_5
EndProc

Proc whitelist()
    num_whitelist = 1
    For i = 0 To 4
ancora:
        Select num_whitelist
            Case 1
                HSerIn 1000,ancora,[Wait("OK")]
            Case 2
                HSerIn 1000,ancora,[Wait("OK")]
            Case 3
                HSerIn 1000,ancora,[Wait("OK")]
            Case 4
                HSerIn 1000,ancora,[Wait("OK")]
            Case 5
                HSerIn 1000,ancora,[Wait("OK")]
        EndSelect
        Inc num_whitelist
    Next i
    num_whitelist = 1
    ancora_1:
    HSerOutLn ["ATE0&W"]
    HSerIn 1000,ancora_1,[Wait("OK")]
EndProc

sim_800_ko:
High led_R
DelayMS 200
Low led_R
DelayMS 200
GoTo sim_800_ko
End

'-----------------Interrupt Routine  -----------------------------------------
Isr:

Context Save
  If RCIF = 1 Then                  ' ricevuto un byte
    rxchar = RCREG                  ' copia il byte, rimuove RCIF automatico
(*  Si esegue un metodo a stadi finiti del processo di riconoscimento di inizio
    e fine della ricezione del messaggio dal modulo. In quanto metodo usato
    per determinare un messaggio e incapsulato tra <messaggio>CRLF CRLF
*)
    If rxchar <> CR Then            ' queello che non e' inizio terminazione
      rxbuffer[bufpntr] = rxchar    ' salva i caratteri
      If bufpntr < BUFLEN Then      ' raccolta caratteri fino al massimo
        Inc bufpntr                 ' prepara al prossimo
      End If
    Else
      Select stage                  ' sceglie a che punto si trova nella rx
        Case 0                      ' appena cominciato
          If rxchar = CR Then       ' un carattere CR?
            stage = 1               ' passa al prossimo stadio al prossimo giro
          Else                      ' oppure
            stage = 0               ' non era un carattere valido
            Clear bufpntr           ' ricomincia dall'inizio
          End If
        Case 1                      ' secondo carattere
          If rxchar = LF Then       ' un LF?
            stage = 2               ' passa al prossimo stadio al prossimo giro
          Else                      ' oppure
            stage = 0               ' non era un carattere valido
            Clear bufpntr           ' ricomincia dall'inizio
          End If
        Case 2
          If rxchar = LF Then       ' un LF?
            stage = 3               ' passa al prossimo stadio al prossimo giro
          Else
            stage = 0
            Clear bufpntr           ' ricomincia dall'inizio
          End If
        Case 3                      ' stadio di completamento ricezione
          Set bufready              ' segnala che ha tutto
          buflast = bufpntr         ' ricorda dove finisce il messaggio
          Clear RCIE
          rxchar = RCREG            ' scarico il buffer HW che rimuove RCIF
          rxchar = RCREG
      End Select
    End If
  End If
Context Restore
