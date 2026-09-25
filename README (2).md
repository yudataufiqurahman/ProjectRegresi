# Dashboard Regresi CCPP

## Cara menjalankan
1. Pastikan 3 file ini berada di **folder yang sama**: `app.R`, `functions_dashboard.R`, `functions_regresi_ccpp.R`.
2. (Opsional) Taruh file data CCPP asli (`.xlsx` / `.ods`, boleh berisi 5 sheet) di folder yang sama atau di sub-folder `data/`. Jika tidak ditemukan, dashboard otomatis memakai data simulasi bergaya CCPP agar semua fitur tetap bisa dicoba.
3. Buka `app.R` di RStudio lalu klik **Run App**, atau jalankan dari konsol:
   ```r
   shiny::runApp("app.R")
   ```
4. Package yang dibutuhkan: `shiny, bslib, DT, plotly, dplyr, tidyr, ggplot2, glmnet, mgcv, readxl, callr` (tambahkan `readODS` jika file sumber berformat `.ods`).

## Struktur 5 Halaman
1. **Eksplorasi Data** — statistik deskriptif, korelasi, distribusi, konsistensi antar-sheet, kualitas data (tanpa upload).
2. **Analisis Model** — persamaan matematis, tabel koefisien, info tuning, kurva tuning, efek parsial, per sheet & per model (tanpa upload).
3. **Evaluasi Model** — perbandingan AICc vs Cross-Validation untuk seluruh sheet & model, stabilitas CV, uji ANOVA/Kruskal-Wallis (tanpa upload).
4. **Simulasi Interaktif** — slider real-time untuk melihat pengaruh parameter (derajat polinomial, alpha/lambda Elastic Net, k/lambda GAM) terhadap kurva model pada dataset dummy.
5. **Upload Data Anda** — unggah CSV/XLSX/XLS/ODS sendiri, pilih target & prediktor, jalankan otomatis ketiga metode, bandingkan hasil AIC vs CV.

## Catatan performa (penting)
Perhitungan Halaman 2 & 3 (analisis penuh 3 model × semua sheet) hanya dijalankan **sekali** lalu di-cache ke folder `cache/` — pemuatan berikutnya hampir instan selama file data tidak berubah. Pada percobaan pertama, proses ini bisa memakan waktu **hingga beberapa menit**, terutama untuk model GAM, karena pencarian parameter smoothing (`mgcv`) kadang butuh waktu lebih lama pada kombinasi data tertentu. Dashboard menampilkan progress bar selama proses ini.

Sebagai pengaman, sistem otomatis:
- Mengambil subsample acak (maks. 2000 baris/sheet, seed tetap) untuk sheet berukuran besar agar performa tetap terjaga (Eksplorasi Data tetap memakai data penuh).
- Jika GAM standar terlalu lama konvergen pada satu sheet, otomatis beralih ke versi lebih ringan, lalu ke regresi linear sebagai jaring pengaman terakhir — dashboard akan **memberi tahu Anda dengan jelas** jika ini terjadi (kotak peringatan kuning di halaman terkait).
- File `functions_regresi_ccpp.R` (metodologi inti Anda) **tidak diubah sama sekali** — semua penyesuaian performa ada di `functions_dashboard.R`.

Untuk menghapus cache dan menghitung ulang dari awal, hapus folder `cache/` di direktori aplikasi.
