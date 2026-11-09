# Seasonality, Box-Jenkins ve Heuristic Plans entegrasyonu

## Eklenen dosyalar

- `R/seasonal_decomposition.R`: Hareketli ortalamalarla klasik mevsimsel ayrıştırma.
- `R/winters.R`: Toplamsal ve çarpımsal Winters (üçlü üstel düzeltme).
- `R/box_jenkins.R`: AR, MA, ARMA ve ARIMA modelleri; manuel veya AIC/BIC tabanlı
  p-d-q seçimi, tahmin aralıkları ve artık tanıları.
- `R/heuristic_utils.R`: Heuristic planların ortak veri, maliyet, grafik ve tablo fonksiyonları.
- `R/chase_plan.R`: Zero-inventory (Chase) Plan.
- `R/level_plan.R`: Constant-workforce (Level) Plan.
- `R/mixed_plan.R`: Düzenli üretim, fazla mesai ve taşeron üretimini birleştiren Mixed Plan.

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
chase_plan_server("chase_plan")
level_plan_server("level_plan")
mixed_plan_server("mixed_plan")
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

Heuristic planlar için:

1. `Aggregate Planning` menüsünün `Heuristic plans` bölümünden Chase, Level veya Mixed Plan'ı seçin.
2. Talep sütununu, başlangıç işgücünü, işçi başına üretimi ve stok hedeflerini girin.
3. Düzenli üretim, işe alma/çıkarma, stok ve backlog maliyetlerini girin.
4. Mixed Plan'da düzenli işgücü payını veya sabit işgücünü; fazla mesai ve taşeron
   kapasiteleriyle birim maliyetlerini belirleyin.
5. `Fractional workforce` ve `Workforce rounded up` seçenekleri arasında geçiş yapın.
6. Maliyet karşılaştırmasını, üretim-talep grafiğini ve dönem tablosunu kontrol edin.

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

Heuristic planlar, varsayılan `Aggregate planning, 6 periods` verisiyle doğrudan
çalışır. Talep değerleri negatif olamaz. Level ve Mixed Plan'da yetersiz kapasite
sonucu oluşan karşılanmamış talep backlog olarak gösterilir ve maliyetlendirilir.

Box-Jenkins modülü, varsayılan `Nile annual flow, 1871-1970` verisiyle doğrudan
çalışır ve yalnızca temel R `stats` paketini kullanır. Otomatik arama üst sınırları
büyütüldükçe denenmesi gereken model sayısı ve çalışma süresi artar.
