# Aggregate Planning (Assignment 3)

Tüm yöntemler **Aggregate Planning** menüsündedir ve aynı **Plan Setup** verisini kullanır.

## Dosyalar

| Dosya | İçerik |
| --- | --- |
| `R/ap_utils.R` | Plan Setup sekmesi (veri, K, tesis verisi, maliyetler), plan değerlendirme, maliyet tablosu, kümülatif grafik |
| `R/chase_plan.R` | Zero-inventory (Chase) Plan |
| `R/level_plan.R` | Constant-workforce (Level) Plan |
| `R/mixed_plan.R` | Mixed Plan: işgücünün değiştiği dönemleri kullanıcı seçer |
| `R/linear_programming.R` | LP modeli (fazla mesai, boş zaman, fason seçenekleri), `lpSolve` |
| `R/transportation_method.R` | Transportation tableau: north-west corner ve cheapest cell first |
| `R/disaggregation.R` | Kare kök kuralı (Bitran-Hax): Y_j ∝ √(K_j λ_j), alt/üst sınırlarla |

## Kullanım

1. **Plan Setup**: Excel/CSV yükleyin (dönem adı, talep, çalışma günü sütunları) veya örnek veri seçin.
   Dönem adları kutuda düzenlenebilir. K'yı doğrudan girin ya da geçmiş veriden
   (üretilen birim / (işçi × gün)) hesaplatın. Başlangıç işgücü, başlangıç ve bitiş stoku ile
   maliyetleri girin. İşçilik maliyeti birim başına veya işçi-gün başına (payroll) olabilir.
2. Her plan sekmesi; işçi, işe alma, çıkarma, üretim ve stok tablosunu, kümülatif üretim -
   kümülatif net talep grafiğini ve kalem kalem maliyeti gösterir.
3. İşçi sayısı kesirliyse "Whole workers (rounded up)" planı orijinalin yanında gösterilir.

## Kitapla doğrulama (Nahmias & Olsen, Example 3.1 Densepack)

| Yöntem | Kitap | Uygulama |
| --- | --- | --- |
| K | 245 / (76 × 22) = 0.14653 | 0.14653 |
| Chase (tam işçi) | 755 işe alma, 145 çıkarma, $572,900 | 755, 145, $572,701 |
| Level | 411 işçi, $580,460 | 411 işçi, $580,356 |
| LP optimum | $379,292.22 | $379,292.22 |
| LP yukarı yuvarlama | 273 → 738 işçi, 465 / 27, $379,500 | aynı işçiler, $379,614 |

Küçük maliyet farkları kitabın üretimi tam birime yuvarlamasından gelir.
Slaytlardaki sabit işgücü örneğinde kesin değer 46.0025 işçidir; uygulama eksik
üretim olmaması için 47'ye yuvarlar (slayt üretimi tam birime yuvarlayarak 46 kullanır).
