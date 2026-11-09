# Seasonality ve Box-Jenkins entegrasyonu

Aggregate Planning (Assignment 3) modülleri için `AGGREGATE_PLANNING_TR.md` dosyasına bakın.

## Eklenen dosyalar

- `R/seasonal_decomposition.R`: Hareketli ortalamalarla klasik mevsimsel ayrıştırma.
- `R/winters.R`: Toplamsal ve çarpımsal Winters (üçlü üstel düzeltme).
- `R/box_jenkins.R`: AR, MA, ARMA ve ARIMA modelleri; manuel veya AIC/BIC tabanlı
  p-d-q seçimi, tahmin aralıkları ve artık tanıları.

Her dosyada önce Shiny'den bağımsız hesaplama fonksiyonları, sonra UI ve server
modülleri bulunur. Bu düzen projedeki `seasonal_factors.R`, `holt.R` ve
`forecast_utils.R` düzeniyle aynıdır.

## Ortak yardımcılar

`R/forecast_utils.R` içine `fc_acf()` ve `fc_suggest_season_length()` eklendi.
Böylece üç mevsimsellik modülü de aynı otokorelasyon tabanlı N önerisini
kullanır. Veri yükleme, hata ölçüleri, grafik, dönem tablosu ve CSV indirme
mevcut ortak fonksiyonlardan gelir.

## app.R bağlantıları

Forecasting menüsüne `Seasonal series` başlığı altında şu sekmeler bağlandı:

1. Seasonal Decomposition
2. Winters' Method

Seasonal Factors (quick and dirty) ödevde "Stationary series" altında istendiği
için `Stationary series` başlığında kalır (trend içermeyen mevsimsel seriler için).

Server bölümünde aşağıdaki modüller çağrılır:

```r
seasonal_decomposition_server("seasonal_decomposition")
winters_server("winters")
box_jenkins_server("box_jenkins")
```

## Kullanım adımları

1. Projeyi RStudio'da açın.
2. Gerekirse `install.packages(c("shiny", "readxl"))` çalıştırın.
3. Konsolda `shiny::runApp()` çalıştırın.
4. `Forecasting > Seasonal series` menüsünden yöntemi seçin.
5. Örnek veri kullanın veya Excel/CSV yükleyin.
6. Mevsim uzunluğu N'yi girin ya da otokorelasyon önerisini seçin.
7. Model tipini seçin. Mevsim etkisi seri seviyesiyle büyüyorsa çarpımsal,
   yaklaşık sabit büyüklükteyse toplamsal model uygundur.
8. Winters için alpha, beta ve gamma değerlerini girin veya hata ölçütüne göre
   aratın.
9. Grafikleri, MAD/MSE/MAPE tablosunu, dönem ayrıntılarını ve gelecek dönem
   tahminlerini kontrol edin.

Box-Jenkins için:

1. `Forecasting > Box-Jenkins > AR, MA, ARMA and ARIMA` sekmesini açın.
2. AR, MA, ARMA veya ARIMA model ailesini seçin.
3. p, d ve q değerlerini elle girin veya `Search for the best order` seçeneğini
   kullanarak AIC/BIC ile aratın.
4. Tahmin ufkunu belirleyin ve katsayılar, %95 tahmin aralıkları, ACF/PACF,
   artık ACF'si ile Ljung-Box sonucunu inceleyin.

## Yöntem özeti

Seasonal Decomposition; merkezlenmiş hareketli ortalamayı, ham mevsimsel
oran/farkları, normalize edilmiş mevsim katsayılarını ve mevsimsellikten
arındırılmış doğrusal trendi ayrı ayrı gösterir.

Winters; ilk mevsimleri hareketli ortalama ayrıştırmasıyla başlatır. Başlatma
döneminden sonraki her gözlemde seviye, trend ve ilgili mevsim katsayısını
günceller. Bu nedenle tek-adım tahmin hataları yalnızca başlatma döneminden
sonra hesaplanır.

## Hızlı kontrol

Varsayılan `Turner Industries quarterly sales` verisi ve N = 4 ile iki modül de
çalışır. Çarpımsal modelde sıfır veya negatif değer kabul edilmez; bu tür bir
seri için toplamsal model seçilmelidir.

Box-Jenkins modülü, varsayılan `Nile annual flow, 1871-1970` verisiyle doğrudan
çalışır ve yalnızca temel R `stats` paketini kullanır. Otomatik arama üst sınırları
büyütüldükçe denenmesi gereken model sayısı ve çalışma süresi artar.
