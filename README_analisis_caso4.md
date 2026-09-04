# Análisis estadístico: comparación Antes vs Después (Caso4)

Este repositorio contiene el análisis estadístico completo, reproducible en R, de una base de datos con **26 sujetos (`Cultivo`)** medidos en dos momentos: **Antes** y **Después** de una intervención/tratamiento.

Como cada sujeto aparece una sola vez con dos mediciones (no son dos grupos de sujetos distintos), este es un **diseño de datos pareados / medidas repetidas**, y esa característica es la que guía todas las decisiones metodológicas de este análisis.

## Contenido

- [Requisitos](#requisitos)
- [Parte 0 — Librerías](#parte-0--librerías)
- [Parte 1 — Carga de datos](#parte-1--carga-de-datos)
- [Parte 2 — Conversión a factor y formato largo](#parte-2--conversión-a-factor-y-formato-largo)
- [Parte 3 — Prueba de normalidad](#parte-3--prueba-de-normalidad-shapiro-wilk)
- [Parte 4 — Boxplot](#parte-4--boxplot-detección-visual-de-outliers)
- [Parte 5 — Prueba de IQR para outliers](#parte-5--prueba-de-iqr-para-outliers)
- [Parte 6 — Eliminación de outliers](#parte-6--eliminación-de-outliers-y-ajuste-de-datos)
- [Parte 7 — Homogeneidad de varianzas](#parte-7--homogeneidad-de-varianzas-homocedasticidad)
- [Parte 8 — Tamaño del efecto (Cohen's d)](#parte-8--tamaño-del-efecto-cohens-d)
- [Parte 9 — Prueba de hipótesis (t de Student pareada)](#parte-9--prueba-de-hipótesis-t-de-student-pareada)
- [Parte 10 — ANOVA (referencia)](#parte-10--anova-de-un-factor-referencia)
- [Parte 11 — Boxplot final](#parte-11--boxplot-final)
- [Análisis estadístico clave](#análisis-estadístico-clave)

---

## Requisitos

```r
install.packages(c("car", "effsize", "lsr"))
```

El archivo de datos `Caso4.csv` debe estar en el mismo directorio de trabajo (o ajusta la ruta en `read.csv()`).

---

## Parte 0 — Librerías

**Por qué:** `car` provee `leveneTest()` para homogeneidad de varianzas, `effsize` provee `cohen.d()` para el tamaño del efecto, y `lsr` ofrece `cohensD()` como alternativa de verificación.

```r
library(car)      # leveneTest()
library(effsize)  # cohen.d()
library(lsr)      # cohensD()
```

---

## Parte 1 — Carga de datos

**Por qué:** siempre se inspecciona la estructura de los datos antes de analizarlos, para confirmar tipos de variable, número de observaciones y detectar problemas de codificación (aquí el CSV trae BOM UTF-8, de ahí `fileEncoding`).

```r
datos <- read.csv("Caso4.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
str(datos)
```

**Interpretación esperada:** `data.frame` de 26 observaciones y 3 variables (`Cultivo` como texto, `Antes` y `Despues` como numéricas).

---

## Parte 2 — Conversión a factor y formato largo

**Por qué:** `Cultivo` es una variable categórica (identificador de sujeto), por lo que se convierte a `factor` para que R la trate como tal y no como texto libre. Además, se reorganizan los datos a **formato largo** (una columna `Grupo` con niveles `Antes`/`Despues` y una columna `Valor`), que es el formato que requieren funciones como `aov()` y `leveneTest()`.

```r
datos$Cultivo <- as.factor(datos$Cultivo)

datos_largo <- data.frame(
  Cultivo = rep(datos$Cultivo, 2),
  Grupo   = factor(rep(c("Antes", "Despues"), each = nrow(datos)),
                    levels = c("Antes", "Despues")),
  Valor   = c(datos$Antes, datos$Despues)
)
```

---

## Parte 3 — Prueba de normalidad (Shapiro-Wilk)

**Por qué:** la mayoría de las pruebas paramétricas (t de Student, ANOVA) asumen que los datos —o, en diseños pareados, las **diferencias**— siguen una distribución normal. Shapiro-Wilk contrasta H₀: los datos provienen de una distribución normal.

```r
shapiro.test(datos$Antes)
shapiro.test(datos$Despues)
shapiro.test(datos$Antes - datos$Despues)   # normalidad de las diferencias
```

**Interpretación de los resultados obtenidos:**

| Variable | W | p-valor | ¿Normal? |
|---|---|---|---|
| Antes | 0.898 | 0.014 | No (p < 0.05) |
| Después | 0.878 | 0.005 | No (p < 0.05) |
| **Diferencias (Antes−Después)** | 0.973 | **0.706** | **Sí (p ≥ 0.05)** |

Antes y Después, tomados por separado, no son normales. Pero en un **diseño pareado eso no es lo que importa**: lo relevante es la normalidad de las diferencias individuales, y esas sí son normales (p = 0.706). Esto habilita usar la t de Student pareada más adelante.

---

## Parte 4 — Boxplot (detección visual de outliers)

**Por qué:** antes de aplicar una regla numérica de outliers, conviene visualizarlos. El boxplot muestra la mediana, los cuartiles y marca con puntos los valores fuera del rango intercuartílico.

```r
boxplot(datos$Antes, datos$Despues,
        names = c("Antes", "Despues"),
        col = c("#8ecae6", "#ffb703"),
        main = "Boxplot - Antes vs Despues",
        ylab = "Valor")
```

**Interpretación:** se observa un punto claramente separado del resto en ambos grupos (mismo sujeto), correspondiente a valores mucho más altos (~37 en Antes, ~34 en Después) que el resto de la muestra.

---

## Parte 5 — Prueba de IQR para outliers

**Por qué:** el boxplot da una pista visual, pero la **regla del rango intercuartílico (IQR)** la formaliza numéricamente: un valor se considera outlier si cae fuera de `[Q1 − 1.5·IQR, Q3 + 1.5·IQR]`.

```r
detectar_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75)
  iqr <- IQR(x)
  which(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
}

out_antes   <- detectar_outliers_iqr(datos$Antes)
out_despues <- detectar_outliers_iqr(datos$Despues)
datos$Cultivo[out_antes]
datos$Cultivo[out_despues]
```

**Interpretación:** el sujeto **P10** queda identificado como outlier en ambas columnas (Antes = 37.198, Después = 33.926), confirmando lo observado en el boxplot.

---

## Parte 6 — Eliminación de outliers y ajuste de datos

**Por qué:** como el diseño es **pareado**, si un sujeto es outlier en cualquiera de las dos condiciones se elimina el **par completo** (las dos observaciones de ese sujeto), no solo el valor aislado — de lo contrario se rompería el pareo y las funciones de t pareada/diferencias dejarían de tener sentido.

```r
filas_outlier <- unique(c(out_antes, out_despues))
datos_limpio  <- if (length(filas_outlier) > 0) datos[-filas_outlier, ] else datos
datos_limpio$Cultivo <- factor(datos_limpio$Cultivo)

datos_largo_limpio <- data.frame(
  Cultivo = rep(datos_limpio$Cultivo, 2),
  Grupo   = factor(rep(c("Antes", "Despues"), each = nrow(datos_limpio)),
                    levels = c("Antes", "Despues")),
  Valor   = c(datos_limpio$Antes, datos_limpio$Despues)
)

# Re-chequeo de normalidad tras limpiar
shapiro.test(datos_limpio$Antes)
shapiro.test(datos_limpio$Despues)
sw_dif <- shapiro.test(datos_limpio$Antes - datos_limpio$Despues)
sw_dif
```

**Interpretación:** se elimina P10, quedando **25 pares**. Tras la limpieza, la normalidad de las diferencias se mantiene (p = 0.776), por lo que el camino paramétrico sigue siendo válido.

---

## Parte 7 — Homogeneidad de varianzas (homocedasticidad)

**Por qué:** aunque la homogeneidad de varianzas es un supuesto propio de comparaciones **independientes** (no del test pareado en sí), se reporta como parte del diagnóstico estándar de todo flujo de comparación de dos grupos, siguiendo el test de Levene (más robusto a no-normalidad que el test F clásico).

```r
leveneTest(Valor ~ Grupo, data = datos_largo_limpio)
var.test(datos_limpio$Antes, datos_limpio$Despues)
```

**Interpretación:** Levene da F = 0.097, p = 0.757 → no se rechaza igualdad de varianzas. El test F de referencia coincide (p = 0.637). Las varianzas de Antes y Después son estadísticamente equivalentes.

---

## Parte 8 — Tamaño del efecto (Cohen's d)

**Por qué:** el p-valor solo dice si hay diferencia o no; el **tamaño del efecto** dice qué tan grande es esa diferencia en términos prácticos, independientemente del tamaño muestral. Para datos pareados se usa la variante `dz` (basada en la desviación estándar de las diferencias).

```r
cohen.d(datos_limpio$Antes, datos_limpio$Despues, paired = TRUE)
cohen.d(datos_limpio$Antes, datos_limpio$Despues, paired = FALSE)
cohensD(datos_limpio$Antes, datos_limpio$Despues, method = "paired")
```

**Interpretación:**

| Métrica | Valor | Magnitud |
|---|---|---|
| d pareado (correcto para este diseño) | **2.21** | **Efecto grande** (> 0.8) |
| d como si fueran independientes (referencia) | 0.44 | Efecto pequeño-mediano |

El d pareado es el que corresponde a este diseño y confirma que el cambio Antes→Después no solo es estadísticamente significativo, sino de **magnitud sustancial**.

---

## Parte 9 — Prueba de hipótesis: t de Student pareada

**Por qué esta prueba y no otra:**
1. Solo hay **2 condiciones** (Antes, Después) → candidato natural es t, no ANOVA (que se reserva para 3+ grupos).
2. Las dos mediciones provienen del **mismo sujeto** → se debe usar la variante **pareada** de la t, que trabaja sobre las diferencias individuales en lugar de tratar ambos grupos como muestras independientes.
3. Las diferencias son normales (Parte 6) → se usa la versión **paramétrica** (t de Student). Si no lo fueran, la alternativa sería `wilcox.test(paired = TRUE)` (Wilcoxon de rangos con signo).

```r
prueba_t <- t.test(datos_limpio$Antes, datos_limpio$Despues,
                    paired = TRUE, alternative = "two.sided")
prueba_t

p_val <- prueba_t$p.value
cat("Valor de p de la t de Student pareada:",
    format.pval(p_val, digits = 3, eps = 0.001), "\n")

if (p_val < 0.001) {
  cat("p < 0.001 -> diferencia altamente significativa entre Antes y Despues\n")
} else if (p_val < 0.05) {
  cat("p < 0.05 -> diferencia significativa entre Antes y Despues\n")
} else {
  cat("p >= 0.05 -> no hay diferencia significativa\n")
}
```

**Interpretación de los resultados obtenidos:**

- t(24) = 11.04, **p = 6.85 × 10⁻¹¹** (p < 0.001)
- Diferencia media = 2.99 unidades
- IC 95% de la diferencia: [2.43, 3.55] — no cruza el cero

Se **rechaza H₀** (igualdad de medias): existe una diferencia real y muy significativa entre Antes y Después. Como el intervalo de confianza es enteramente positivo, la dirección del efecto es clara: los valores **disminuyen** después de la intervención.

---

## Parte 10 — ANOVA de un factor (referencia)

**Por qué se incluye pero no es la prueba elegida:** con solo 2 grupos, ANOVA es matemáticamente equivalente a t² (`F = t²`) y **no** incorpora la estructura pareada de los datos (trataría Antes y Después como si fueran de sujetos distintos), por lo que subestima la significancia real. Se muestra únicamente con fines ilustrativos/comparativos.

```r
anova_modelo <- aov(Valor ~ Grupo, data = datos_largo_limpio)
resumen_anova <- summary(anova_modelo)
resumen_anova

p_anova <- resumen_anova[[1]][["Pr(>F)"]][1]
cat("Valor de p de la ANOVA (referencia, no pareada):",
    format.pval(p_anova, digits = 3, eps = 0.001), "\n")

ss <- resumen_anova[[1]][["Sum Sq"]]
eta_sq <- ss[1] / sum(ss)
cat("Eta cuadrado (ANOVA, referencia):", round(eta_sq, 3), "\n")
```

**Interpretación:** F(1,48) = 2.38, p = 0.129 (**no significativa**) — muy distinto al resultado de la t pareada (p < 0.001). Esto ilustra exactamente el riesgo de ignorar la estructura pareada: al tratar las 50 observaciones como si vinieran de 50 sujetos independientes, se pierde toda la información de que cada par pertenece al mismo sujeto, y la prueba pierde poder estadístico drásticamente.

> **Nota:** no se aplica prueba post-hoc porque los post-hoc (Tukey, Bonferroni, etc.) sirven para desambiguar **cuál par de 3+ grupos difiere**; con solo 2 grupos, la comparación directa (t o ANOVA) ya es la comparación completa.

---

## Parte 11 — Boxplot final

**Por qué:** verificar visualmente que, tras eliminar el outlier, la distribución de ambos grupos es más homogénea y consistente con los supuestos usados en las pruebas.

```r
boxplot(Valor ~ Grupo, data = datos_largo_limpio,
        col = c("#8ecae6", "#ffb703"),
        main = "Antes vs Despues (sin outliers)",
        ylab = "Valor")
```

---

## Análisis estadístico clave

1. **Diseño correcto identificado:** los datos son pareados (mismo sujeto, dos mediciones), no dos grupos independientes. Esta decisión metodológica temprana condiciona todas las pruebas posteriores.
2. **Un outlier real y consistente (P10)**, presente en ambas mediciones, fue eliminado como par completo para no distorsionar la estimación de las diferencias ni de la varianza.
3. **Los supuestos del test elegido se cumplen:** las diferencias Antes−Después son normales (p = 0.776) y las varianzas son homogéneas (Levene p = 0.757), validando el uso de la t de Student pareada.
4. **Efecto grande y consistente:** Cohen's d pareado = 2.21, muy por encima del umbral convencional de "efecto grande" (0.8). No es solo significativo estadísticamente: es relevante en magnitud práctica.
5. **Resultado principal:** t(24) = 11.04, p < 0.001, diferencia media = 2.99 (IC95%: 2.43–3.55). Se concluye que la intervención produjo una **reducción real, sistemática y de gran magnitud** en la variable medida.
6. **Advertencia metodológica importante:** al analizar los mismos datos ignorando el pareo (ANOVA de un factor tratando Antes/Después como grupos independientes), el resultado deja de ser significativo (p = 0.129). Esto demuestra en la práctica por qué **la elección del test debe basarse en la estructura del diseño experimental**, y no solo en el número de grupos: usar la prueba equivocada puede llevar a una conclusión opuesta (falso negativo) a partir de exactamente los mismos datos.

---

### Cómo reproducir este análisis

```bash
git clone <url-del-repositorio>
cd <repositorio>
# Colocar Caso4.csv en el directorio
Rscript analisis_caso4.R
```

O bien, copiar cada bloque de código de este documento en una sesión de R/RStudio, en el mismo orden en que aparecen (cada parte depende de los objetos creados en las partes anteriores).
