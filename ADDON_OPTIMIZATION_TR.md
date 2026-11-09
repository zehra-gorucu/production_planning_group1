# Sadece Kalan 3 Modül — Yükleme Rehberi

Bu paket, GitHub'daki güncel projeye **yalnızca aşağıdaki üç yeni bölümü** ekler:

1. **Linear Programming**
   - normal üretim, fazla mesai, boş zaman ve fason üretim seçenekleri;
   - iş gücü, işe alma/çıkarma, stok ve backlog dengeleri;
   - toplam maliyeti en küçük yapan sürekli veya tam sayılı iş gücü planı.
2. **Transportation Method**
   - her dönemin normal zaman, fazla mesai ve fason üretimi ayrı kaynaklardır;
   - North-West Corner başlangıç çözümü;
   - Cheapest Cell First çözümü;
   - karşılaştırma için gerçek minimum maliyetli LP çözümü.
3. **Disaggregation**
   - aggregate planı ürün ailelerine kare-kök kuralıyla dağıtır;
   - kesirli veya toplamı koruyan tam sayılı dağıtım verir.

Önceki Forecasting, Box–Jenkins ve Heuristic Plan dosyaları bu pakette tekrar yoktur.

## ZIP içindeki dosyalar

- `app.R` — mevcut dosyanın güncellenmiş hâli; üç yeni sekmeyi menüye bağlar.
- `manifest.json` — yeni dosyaları ve `lpSolve` paketini yayına ekler.
- `R/linear_programming.R` — Linear Programming modülü.
- `R/transportation_method.R` — Transportation Method modülü.
- `R/disaggregation.R` — Disaggregation modülü.
- `ADDON_OPTIMIZATION_TR.md` — bu açıklama; GitHub'a yüklemek zorunlu değildir.

## GitHub'a yükleme

ZIP'i önce Mac'te açın. Sonra arkadaşınızın aynı GitHub deposunda:

1. Ana dizine girin ve **Add file → Upload files** seçeneğine basın.
2. Yeni `app.R` ve `manifest.json` dosyalarını yükleyin. GitHub aynı adlı eski dosyaların yerine güncel sürümü koyacaktır.
3. GitHub'da `R` klasörüne girin.
4. **Add file → Upload files** ile yalnızca şu üç dosyayı yükleyin:
   - `linear_programming.R`
   - `transportation_method.R`
   - `disaggregation.R`
5. **Commit changes** düğmesine basın.

`R` klasöründeki eski dosyaları silmeyin; yeni modüller ortak veri okuma ve grafik yardımcılarını onlardan kullanır.

## RStudio'da çalıştırma

İlk kez yalnızca bir defa Console'a şunu yazın:

```r
install.packages("lpSolve")
```

Ardından proje klasöründeki `app.R` dosyasını açın ve sağ üstteki **Run App** düğmesine basın. Üç modül **Aggregate Planning** menüsünde görünür: Linear Programming ve Transportation Method "Optimization" başlığı altında, Disaggregation kendi başlığı altında.

Posit/ShinyApps'e yeniden Publish edildiğinde güncel `manifest.json`, `lpSolve` paketinin sunucuda kurulmasını sağlar.
