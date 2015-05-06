!Este programa es para leer datos acerca del nivel del mar en cierta costa, contenidos en un archivo .dat
!para darles formato y poder detectar y graficar las mareas diurna, semidiurna y mixta; en esta costa
!los datos fuero tomados y registrados por un aparato de moritoreo, cada media hora a lo largo de       
!aproximadamente 4 meses, para poder graficar se hizo un promedio del nivel del mar en cada hora

PROGRAM Mareas
IMPLICIT NONE

INTEGER :: OpenStatus, InputStatus
CHARACTER(20) :: FileName
REAL ::  Hora, Nivel, Numero

WRITE (*, '(1X, A)', ADVANCE = "NO") "Enter name of the data file:"
READ *, FileName
OPEN (UNIT= 12, FILE = FileName, STATUS = "OLD", ACTION = "READ", POSITION = "REWIND",  IOSTAT = OpenStatus)
IF (Openstatus > 0) STOP "***Cannot open the file***"

READ (12, *)Hora, Nivel, Numero
 



OPEN (UNIT = 13, FILE = "Formato", STATUS = "NEW", ACTION = "WRITE", IOSTAT = OpenStatus)

WRITE (13, *) Numero, Nivel

CLOSE (13)

END PROGRAM Mareas

