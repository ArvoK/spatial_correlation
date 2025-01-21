import numpy as np
from tabulate import tabulate

# Einlesen
koord_s = np.array([
    [0, 7513858.42, 10025291.04, 23435977.09],  # Punkt A
    [0, 17164353.02, 14725292.31, 13964732.96],  # Punkt B
    [0, 7907487.12, 16025292.25, 19664733.86],  # Punkt C
    [0, 10861899.32, 13591958.95, 19021814.35]   # Punkt D
])
str_s = np.array([
    [0, 21077225.89],  # Strecke DA
    [0, 21021306.93],  # Strecke DB
    [0, 21532371.13],  # Strecke DC
    [0, 20205032.62]   # Strecke DD
])

# Normalize coordinates to avoid overflow
koord_s[:, 1:] /= 1e6
str_s[:, 1] /= 1e6

# Matrizen bilden
N = koord_s.shape[0]
if N != str_s.shape[0]:
    print('Anzahl der Messungen überprüfen!')
    exit()

# Erste Näherungslösung für die Standpunktkoordinaten
xN, yN, zN = 4, 1, 5  # Adjusted for normalization

# Iterationen zur Verbesserung der Näherungslösung
Grenze = float(input('Grenzwert bei den Iterationen = '))
Korr = np.array([100, 100, 100])
Anz = 10

# Tabelle initialisieren
table = []

while np.any(np.abs(Korr) > Grenze):
    # Berechnung von Näherungsstrecken
    str_n = np.sqrt((koord_s[:, 1] - xN)**2 + (koord_s[:, 2] - yN)**2 + (koord_s[:, 3] - zN)**2)

    # Bilden von Matrizen
    A = (koord_s[:, 1:4] - np.array([xN, yN, zN])) / str_n[:, np.newaxis]
    B = str_s[:, 1] - str_n

    # Berechnung von Korrekturen
    Norm = A.T @ A
    Q = np.linalg.pinv(Norm)  # Use pseudo-inverse instead of inverse
    HW = Q @ A.T
    Korr = HW @ B

    xN, yN, zN = xN + Korr[0], yN + Korr[1], zN + Korr[2]
    Anz += 1

    # Werte zur Tabelle hinzufügen
    table.append([Anz, xN * 1e6, yN * 1e6, zN * 1e6, Korr[0], Korr[1], Korr[2]])

# Tabelle ausgeben
print(tabulate(table, headers=["Iteration", "xN", "yN", "zN", "Korr_x", "Korr_y", "Korr_z"]))

# Ausgabe der finalen Werte
print(f"Finale Werte: xN = {xN * 1e6}, yN = {yN * 1e6}, zN = {zN * 1e6}")