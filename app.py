from pathlib import Path
import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import json
import geopandas as gpd

DATA_DIR = Path(__file__).parent

# ── Page config ───────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="Tanzania Vaccine Coverage Dashboard",
    page_icon="💉",
    layout="wide",
    initial_sidebar_state="expanded",
)

# ── Custom CSS ─────────────────────────────────────────────────────────────────
st.markdown("""
<style>
    .main-title { font-size: 2rem; font-weight: 700; color: #1a5276; margin-bottom: 0; }
    .sub-title  { font-size: 1rem; color: #555; margin-top: 0.1rem; margin-bottom: 1.5rem; }
    .metric-card {
        background: #f0f4fa; border-radius: 10px; padding: 1rem 1.2rem;
        border-left: 4px solid #2874a6; margin-bottom: 0.5rem;
    }
    .metric-card h4 { margin: 0; font-size: 0.85rem; color: #555; }
    .metric-card h2 { margin: 0; font-size: 1.6rem; color: #1a5276; }
    .section-header { font-size: 1.2rem; font-weight: 600; color: #1a5276;
                      border-bottom: 2px solid #d0dce9; padding-bottom: 0.3rem;
                      margin-top: 1.5rem; margin-bottom: 1rem; }
    .chart-helper { background: #f8f9fa; border-left: 3px solid #2874a6;
                    padding: 0.6rem 1rem; border-radius: 0 6px 6px 0;
                    color: #444; font-size: 0.88rem; margin-bottom: 0.8rem; }
    .insight-box { background: #eaf4fb; border-radius: 8px; padding: 0.8rem 1.2rem;
                   margin: 0.5rem 0 1rem 0; border: 1px solid #aed6f1; }
    .insight-box p { margin: 0.2rem 0; font-size: 0.9rem; color: #1a5276; }
    .insight-box .insight-title { font-weight: 700; font-size: 0.95rem;
                                   color: #1a5276; margin-bottom: 0.4rem; }
    .finding-item { padding: 0.3rem 0; border-bottom: 1px solid #d6eaf8;
                    font-size: 0.9rem; color: #333; }
    .finding-item:last-child { border-bottom: none; }
    .glossary-term { font-weight: 600; color: #1a5276; margin-bottom: 0.1rem; }
    .glossary-def  { color: #444; font-size: 0.9rem; margin-bottom: 0; }
</style>
""", unsafe_allow_html=True)

# ── Data loading ───────────────────────────────────────────────────────────────
@st.cache_data
def load_coverage():
    p1 = pd.read_excel(DATA_DIR / "task1_coverage.xlsx", sheet_name="Penta1Cov")
    p3 = pd.read_excel(DATA_DIR / "task1_coverage.xlsx", sheet_name="Penta3Cov")
    for df in [p1, p3]:
        for col in ["Coverage", "95% CI Lower", "95% CI Upper", "CI_Lower", "CI_Upper"]:
            if col in df.columns:
                df[col] = df[col] / 100
    p1 = p1.rename(columns={"95% CI Lower": "CI_Lower", "95% CI Upper": "CI_Upper"})
    p3 = p3.rename(columns={"95% CI Lower": "CI_Lower", "95% CI Upper": "CI_Upper"})
    for df in [p1, p3]:
        df["Region"] = df["Region"].str.title()
    name_map = {"Dar Es Salaam": "Dar-es-salaam"}
    for df in [p1, p3]:
        df["Region"] = df["Region"].replace(name_map)
    return p1, p3

@st.cache_data
def load_outliers():
    df = pd.read_excel(DATA_DIR / "task2_outlier_summary.xlsx", sheet_name="Outlier Summary")
    df.columns = [
        "admin2", "indicator_type", "year",
        "total_entries", "outliers_sd", "outliers_mad",
        "pct_sd", "pct_mad", "pct_sd2", "pct_mad2"
    ]
    df["indicator_label"] = df["indicator_type"].map(
        {"penta1_u1": "Penta 1", "penta3_u1": "Penta 3"}
    )
    return df

@st.cache_data
def load_geo():
    gdf = gpd.read_file(DATA_DIR / "DataFiles" / "tza_adm2_map.geojson")
    adm1 = gdf.dissolve(by="ADM1_EN").reset_index()[["ADM1_EN", "geometry"]]
    return adm1

penta1, penta3 = load_coverage()
outliers   = load_outliers()
adm1_geo   = load_geo()

# ── Pre-compute key stats for dynamic insights ─────────────────────────────────
p1_avg        = penta1["Coverage"].mean()
p3_avg        = penta3["Coverage"].mean()
dropout       = p1_avg - p3_avg
n_below_90_p1 = (penta1["Coverage"] < 90).sum()
n_below_90_p3 = (penta3["Coverage"] < 90).sum()
n_below_70_p3 = (penta3["Coverage"] < 70).sum()
best_p3       = penta3.loc[penta3["Coverage"].idxmax(), "Region"]
worst_p3      = penta3.loc[penta3["Coverage"].idxmin(), "Region"]

# ── Shared helpers ────────────────────────────────────────────────────────────
GLOSSARY = [
    ("Penta 1",
     "The first dose of the pentavalent vaccine, which protects against five diseases: diphtheria, "
     "tetanus, whooping cough, hepatitis B, and Hib meningitis. Penta 1 coverage tells us what "
     "share of children received at least their first dose."),
    ("Penta 3",
     "The third and final recommended dose of the pentavalent vaccine. Completing all three doses "
     "provides full protection. Lower Penta 3 than Penta 1 coverage means children are starting "
     "but not finishing the vaccine series."),
    ("Dropout Rate",
     "The percentage-point difference between Penta 1 and Penta 3 coverage. A higher dropout rate "
     "means more children are missing their follow-up doses. A dropout above 10 percentage points "
     "is generally considered a concern."),
    ("Survey-Weighted Coverage",
     "Coverage estimates adjusted to account for how the survey sample was drawn, ensuring results "
     "represent the full population rather than just those surveyed. This makes regional comparisons "
     "fair even where sample sizes differ."),
    ("95% Confidence Interval (CI)",
     "A range of values within which the true coverage figure is likely to fall 95% of the time. "
     "Wider intervals indicate more uncertainty — usually due to smaller sample sizes in that region."),
    ("WHO 90% Target",
     "The World Health Organization's recommended minimum coverage threshold for routine immunisation. "
     "Regions below 90% are considered to have insufficient vaccine coverage."),
    ("HMIS",
     "Health Management Information System — the routine administrative system used by health "
     "facilities to record monthly service delivery data, including the number of vaccines given. "
     "Unlike survey data, HMIS covers all facilities continuously but can have reporting gaps or errors."),
    ("DHS",
     "Demographic and Health Survey — a nationally representative household survey conducted "
     "periodically. It collects data directly from caregivers (e.g. reviewing child health cards), "
     "making it independent of facility reporting."),
    ("Outlier",
     "A data point that is unusually high or low compared to the typical pattern for that facility "
     "and indicator. Outliers may reflect data entry errors, stock-outs, catch-up campaigns, or "
     "genuine service delivery spikes."),
    ("SD / Z-score Method",
     "Flags a value as an outlier if it is more than 3 standard deviations from the average. "
     "It is sensitive to extreme values because the average itself can be pulled by unusual data points."),
    ("MAD Method",
     "Median Absolute Deviation — uses the median (middle value) instead of the average. "
     "More robust to existing extreme values, making it better suited to health data with "
     "occasional legitimate spikes."),
    ("Admin1 / Admin2",
     "Administrative levels of geography. Admin1 = regions (Tanzania has 31). "
     "Admin2 = districts within regions. This dashboard uses Admin1 for coverage mapping "
     "and Admin2 for outlier analysis."),
]

def glossary_expander():
    with st.expander("📖 Glossary — Key Terms & Definitions"):
        st.markdown("Plain-English definitions for all metrics and terms used in this dashboard.")
        st.markdown("")
        for term, defn in GLOSSARY:
            st.markdown(
                f'<p class="glossary-term">{term}</p>'
                f'<p class="glossary-def">{defn}</p>'
                f'<hr style="margin:0.5rem 0; border-color:#e8eef4"/>',
                unsafe_allow_html=True,
            )

def chart_helper(text):
    st.markdown(f'<div class="chart-helper">💡 {text}</div>', unsafe_allow_html=True)

def insight_box(title, findings):
    items = "".join(f'<p class="finding-item">• {f}</p>' for f in findings)
    st.markdown(
        f'<div class="insight-box">'
        f'<p class="insight-title">🔍 {title}</p>{items}'
        f'</div>',
        unsafe_allow_html=True,
    )

# ── Sidebar ────────────────────────────────────────────────────────────────────
st.sidebar.image(
    "https://upload.wikimedia.org/wikipedia/commons/3/38/Flag_of_Tanzania.svg",
    width=180,
)
st.sidebar.markdown("## Navigation")
page = st.sidebar.radio(
    "",
    ["📊 Coverage Overview", "🗺️ Choropleth Map", "🔍 Outlier Analysis"],
    label_visibility="collapsed",
)
st.sidebar.markdown("---")
st.sidebar.markdown(
    "**Data sources**  \n"
    "• Tanzania DHS survey data  \n"
    "• Administrative HMIS data  \n"
    "• Tanzania ADM2 boundary GeoJSON"
)
st.sidebar.markdown("---")
st.sidebar.markdown(
    "**How to use this dashboard**  \n"
    "📊 *Coverage* — Explore regional vaccine uptake and compare Penta 1 vs 3  \n"
    "🗺️ *Map* — See geographic patterns in coverage  \n"
    "🔍 *Outliers* — Assess HMIS data quality by district and year"
)


# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 1 – Coverage Overview
# ═══════════════════════════════════════════════════════════════════════════════
if page == "📊 Coverage Overview":
    st.markdown('<p class="main-title">Tanzania Vaccine Coverage Dashboard</p>', unsafe_allow_html=True)
    st.markdown('<p class="sub-title">Survey-weighted Penta 1 & Penta 3 coverage by subnational region · Children 12–23 months</p>', unsafe_allow_html=True)

    # ── About expander ────────────────────────────────────────────────────────
    with st.expander("ℹ️ About this dashboard — click to expand", expanded=True):
        st.markdown("""
**What this dashboard shows**

This dashboard presents vaccine coverage estimates for Tanzania, focusing on the pentavalent vaccine
(Penta 1 and Penta 3) among children aged 12–23 months. It draws on two complementary data sources:

- **DHS Survey data** *(Coverage & Map tabs)*: Nationally representative household survey estimates,
  statistically weighted to reflect the true population. These are the gold-standard figures for
  understanding how many children are actually protected.
- **HMIS administrative data** *(Outlier tab)*: Routine monthly facility reports used to monitor
  service delivery in real time. This data can contain errors or gaps that need quality checking before use.

**What you can learn here**
- Which regions are meeting the WHO 90% immunisation target — and which are falling short
- How many children start the vaccine series (Penta 1) but do not complete it (Penta 3)
- Where HMIS data quality issues are concentrated and whether they have worsened over time

**How to read the charts**
- **Bar charts**: Each bar is one region. Longer = higher coverage. Error bars show the 95% confidence interval.
- **Choropleth map**: Darker blue = higher coverage. Red/orange = below target. Hover for exact figures.
- **Outlier trend line**: Shows the % of monthly reports flagged as suspicious. Rising = worsening data quality.
        """)

    # ── Key Findings banner ───────────────────────────────────────────────────
    insight_box("Key Findings at a Glance", [
        f"National Penta 1 coverage averages {p1_avg:.1f}% — {n_below_90_p1} of 31 regions are below the 90% WHO target.",
        f"National Penta 3 coverage averages {p3_avg:.1f}% — {n_below_90_p3} of 31 regions fall short of the 90% target.",
        f"The national Penta 1→3 dropout rate is {dropout:.1f} percentage points — roughly 1 in {max(1, int(round(100/dropout)))} children who start the series do not complete it.",
        f"{n_below_70_p3} region(s) have Penta 3 coverage below 70%, representing highest-priority areas for intervention.",
        f"Best performing region (Penta 3): {best_p3}. Most in need of attention: {worst_p3}.",
    ])

    # ── KPI row ───────────────────────────────────────────────────────────────
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.markdown(f"""<div class="metric-card">
            <h4>Penta 1 — National Avg</h4>
            <h2>{p1_avg:.1f}%</h2></div>""", unsafe_allow_html=True)
    with col2:
        st.markdown(f"""<div class="metric-card">
            <h4>Penta 3 — National Avg</h4>
            <h2>{p3_avg:.1f}%</h2></div>""", unsafe_allow_html=True)
    with col3:
        st.markdown(f"""<div class="metric-card">
            <h4>Penta 1→3 Dropout</h4>
            <h2>{dropout:.1f} pp</h2></div>""", unsafe_allow_html=True)
    with col4:
        st.markdown(f"""<div class="metric-card">
            <h4>Regions &lt; 70% Penta 3</h4>
            <h2>{n_below_70_p3} / {len(penta3)}</h2></div>""", unsafe_allow_html=True)

    # ── Coverage bar chart ────────────────────────────────────────────────────
    st.markdown('<div class="section-header">Coverage by Region with 95% Confidence Interval</div>', unsafe_allow_html=True)
    chart_helper(
        "Each bar shows estimated vaccine coverage for one region, sorted lowest to highest. "
        "The thin lines at each bar's tip are the 95% confidence interval — the plausible range around the estimate. "
        "The red dashed line marks the WHO 90% target. "
        "Colour coding: 🔴 red = below 70% (critical), 🟠 orange = 70–80% (at risk), 🔵 blue = above 80%."
    )

    vaccine = st.selectbox("Select indicator", ["Penta 1", "Penta 3"])
    df_sel  = penta1.copy() if vaccine == "Penta 1" else penta3.copy()
    df_sel  = df_sel.sort_values("Coverage", ascending=True)
    n_below = (df_sel["Coverage"] < 90).sum()

    fig = go.Figure()
    fig.add_trace(go.Bar(
        y=df_sel["Region"],
        x=df_sel["Coverage"],
        orientation="h",
        marker_color=[
            "#e74c3c" if v < 70 else "#f39c12" if v < 80 else "#2874a6"
            for v in df_sel["Coverage"]
        ],
        error_x=dict(
            type="data", symmetric=False,
            array=df_sel["CI_Upper"] - df_sel["Coverage"],
            arrayminus=df_sel["Coverage"] - df_sel["CI_Lower"],
            color="#555",
        ),
        hovertemplate=(
            "<b>%{y}</b><br>"
            f"{vaccine} coverage: %{{x:.1f}}%<br>"
            "95% CI: %{customdata[0]:.1f}% – %{customdata[1]:.1f}%<extra></extra>"
        ),
        customdata=list(zip(df_sel["CI_Lower"], df_sel["CI_Upper"])),
    ))
    fig.add_vline(x=90, line_dash="dash", line_color="red",
                  annotation_text="90% WHO target", annotation_position="top right")
    fig.update_layout(
        xaxis_title="Coverage (%)", yaxis_title="",
        height=730, margin=dict(l=10, r=30, t=10, b=40),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
        xaxis=dict(range=[0, 108], gridcolor="#dde6f0"),
    )
    st.plotly_chart(fig, use_container_width=True)

    insight_box(f"{vaccine} Coverage — What this chart tells us", [
        f"{n_below} of 31 regions fall below the 90% WHO target for {vaccine}.",
        "Wide error bars (long horizontal lines) indicate smaller survey samples — treat those estimates with more caution.",
        "Regions coloured red (below 70%) should be prioritised for targeted outreach and supply chain review.",
    ])

    # ── Side-by-side comparison ───────────────────────────────────────────────
    st.markdown('<div class="section-header">Penta 1 vs Penta 3 Comparison</div>', unsafe_allow_html=True)
    chart_helper(
        "This chart overlays Penta 1 (darker blue) and Penta 3 (lighter blue) for each region. "
        "The visible gap between the two bars is the dropout — children who received dose 1 but not dose 3. "
        "Regions are sorted by Penta 3 coverage (lowest at top), so the largest gaps appear where they matter most."
    )

    merged = penta1[["Region", "Coverage"]].rename(columns={"Coverage": "Penta1"}).merge(
        penta3[["Region", "Coverage"]].rename(columns={"Coverage": "Penta3"}), on="Region"
    ).sort_values("Penta3")
    merged["Dropout"] = (merged["Penta1"] - merged["Penta3"]).round(1)
    high_dropout = merged[merged["Dropout"] > 10]["Region"].tolist()

    fig2 = go.Figure()
    fig2.add_trace(go.Bar(name="Penta 1", y=merged["Region"], x=merged["Penta1"],
                          orientation="h", marker_color="#2874a6",
                          hovertemplate="<b>%{y}</b><br>Penta 1: %{x:.1f}%<extra></extra>"))
    fig2.add_trace(go.Bar(name="Penta 3", y=merged["Region"], x=merged["Penta3"],
                          orientation="h", marker_color="#a9cce3",
                          hovertemplate="<b>%{y}</b><br>Penta 3: %{x:.1f}%<extra></extra>"))
    fig2.update_layout(
        barmode="overlay", height=730, xaxis_title="Coverage (%)",
        xaxis=dict(range=[0, 108], gridcolor="#dde6f0"),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
        legend=dict(orientation="h", yanchor="bottom", y=1.01, xanchor="right", x=1),
        margin=dict(l=10, r=30, t=10, b=40),
    )
    fig2.add_vline(x=90, line_dash="dash", line_color="red")
    st.plotly_chart(fig2, use_container_width=True)

    dropout_findings = [
        f"The national average dropout from Penta 1 to Penta 3 is {dropout:.1f} percentage points.",
    ]
    if high_dropout:
        dropout_findings.append(
            f"{len(high_dropout)} region(s) exceed a 10 pp dropout: {', '.join(high_dropout)}. "
            "These may face supply chain, distance, or caregiver follow-up challenges."
        )
    else:
        dropout_findings.append(
            "No regions exceed a 10 pp dropout — follow-up completion is relatively consistent nationally."
        )
    dropout_findings.append(
        "A consistent gap across all regions suggests a systemic issue (e.g. scheduling or reminder systems), "
        "whereas isolated large gaps point to local factors."
    )
    insight_box("Penta 1→3 Dropout — What to look for", dropout_findings)

    with st.expander("📋 View raw coverage data"):
        combined = penta1[["Region", "Coverage", "CI_Lower", "CI_Upper"]].copy()
        combined.columns = ["Region", "Penta1 (%)", "P1 CI Lower", "P1 CI Upper"]
        combined = combined.merge(
            penta3[["Region", "Coverage", "CI_Lower", "CI_Upper"]].rename(
                columns={"Coverage": "Penta3 (%)", "CI_Lower": "P3 CI Lower", "CI_Upper": "P3 CI Upper"}),
            on="Region"
        )
        combined["Dropout (pp)"] = (combined["Penta1 (%)"] - combined["Penta3 (%)"]).round(1)
        for col in ["Penta1 (%)", "P1 CI Lower", "P1 CI Upper", "Penta3 (%)", "P3 CI Lower", "P3 CI Upper"]:
            combined[col] = combined[col].round(1)
        st.dataframe(combined.sort_values("Region"), use_container_width=True, hide_index=True)

    glossary_expander()


# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 2 – Choropleth Map
# ═══════════════════════════════════════════════════════════════════════════════
elif page == "🗺️ Choropleth Map":
    st.markdown('<p class="main-title">Vaccine Coverage Map — Tanzania</p>', unsafe_allow_html=True)
    st.markdown('<p class="sub-title">Survey-weighted coverage merged with ADM1 boundary data</p>', unsafe_allow_html=True)

    with st.expander("ℹ️ How to read this map", expanded=False):
        st.markdown("""
**Colour scale:** The map uses a red-to-blue gradient tied to coverage levels.
- 🔴 **Red / Orange** — coverage is low (below ~70%), indicating high need for intervention
- 🟡 **Yellow** — moderate coverage, approaching but not yet at the 90% target
- 🔵 **Blue** — higher coverage; darker blue = closer to or above 90%

**Interacting with the map**
- **Hover** over any region to see the exact coverage estimate and 95% confidence interval
- Use the **indicator selector** above the map to switch between Penta 1 and Penta 3
- Compare the two indicators to identify where the geographic dropout pattern is most pronounced

**What to look for**
- Clusters of red/orange regions may indicate systemic supply chain or geographic access challenges
- Isolated low-coverage regions surrounded by higher-performing neighbours may point to local management issues
- Coastal and island regions (Zanzibar) may have different patterns from the mainland
        """)

    map_vaccine = st.selectbox("Select indicator", ["Penta 1", "Penta 3"], key="map_v")
    df_map = penta1 if map_vaccine == "Penta 1" else penta3

    geo_merged = adm1_geo.merge(df_map[["Region", "Coverage", "CI_Lower", "CI_Upper"]],
                                left_on="ADM1_EN", right_on="Region", how="left")
    geojson_data = json.loads(geo_merged.to_json())

    fig_map = px.choropleth(
        geo_merged, geojson=geojson_data, locations=geo_merged.index,
        color="Coverage", hover_name="ADM1_EN",
        hover_data={"Coverage": ":.1f", "CI_Lower": ":.1f", "CI_Upper": ":.1f"},
        color_continuous_scale=[
            [0.0, "#d73027"], [0.4, "#f46d43"], [0.55, "#fee090"],
            [0.7, "#abd9e9"], [0.85, "#74add1"], [1.0, "#1a5276"],
        ],
        labels={"Coverage": "Coverage (%)"},
        range_color=[50, 100],
    )
    fig_map.update_geos(fitbounds="locations", visible=False)
    fig_map.update_layout(
        margin=dict(l=0, r=0, t=0, b=0), height=580,
        paper_bgcolor="rgba(0,0,0,0)",
        coloraxis_colorbar=dict(title="Coverage (%)", ticksuffix="%"),
    )
    st.plotly_chart(fig_map, use_container_width=True)

    geo_data  = geo_merged.dropna(subset=["Coverage"])
    n_target  = (geo_data["Coverage"] >= 90).sum()
    n_red     = (geo_data["Coverage"] < 70).sum()
    lowest_r  = geo_data.loc[geo_data["Coverage"].idxmin(), "ADM1_EN"]
    highest_r = geo_data.loc[geo_data["Coverage"].idxmax(), "ADM1_EN"]

    insight_box(f"{map_vaccine} — Geographic Summary", [
        f"{n_target} of {len(geo_data)} regions meet the 90% WHO target (shown in blue).",
        f"{n_red} region(s) fall below 70% coverage (shown in red/orange) — highest priority for intervention.",
        f"Lowest coverage: {lowest_r} ({geo_data['Coverage'].min():.1f}%). Highest: {highest_r} ({geo_data['Coverage'].max():.1f}%).",
        "Geographic clusters of low coverage may reflect regional supply chain or access barriers beyond individual facility performance.",
    ])

    st.info(
        "**Note:** Coverage values are survey-weighted DHS estimates for children aged 12–23 months. "
        "Grey regions (if any) indicate no data match between the survey and boundary files."
    )

    unmatched = geo_merged[geo_merged["Coverage"].isna()]["ADM1_EN"].tolist()
    if unmatched:
        st.warning(f"Regions without matched coverage data: {', '.join(unmatched)}")

    glossary_expander()


# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 3 – Outlier Analysis
# ═══════════════════════════════════════════════════════════════════════════════
elif page == "🔍 Outlier Analysis":
    st.markdown('<p class="main-title">HMIS Data Quality — Outlier Assessment</p>', unsafe_allow_html=True)
    st.markdown('<p class="sub-title">Routine administrative data · Outliers detected via SD (Z-score &gt;3) and MAD (score &gt;5)</p>', unsafe_allow_html=True)

    with st.expander("ℹ️ About this analysis — click to expand", expanded=False):
        st.markdown("""
**What this page shows**

Health facilities submit monthly reports to the HMIS recording how many vaccines were administered.
This page assesses the quality of those reports by identifying *outliers* — months where a facility's
reported number was unusually high or low compared to its own historical pattern.

Outliers can arise from:
- **Data entry errors** (e.g. an accidental extra zero)
- **Catch-up campaigns** (a genuine surge in vaccinations)
- **Stock-outs** (zero or near-zero reporting due to vaccine shortages)
- **Reporting delays or corrections** (data entered late or adjusted retrospectively)

**Two detection methods are shown:**

| Method | How it works | Sensitivity |
|--------|-------------|-------------|
| **SD** (Z-score > 3) | Flags values >3 standard deviations from the mean | Higher — more sensitive |
| **MAD** (score > 5) | Flags values far from the median | Lower — more robust to skew |

**Practical guidance:** Districts flagged by *both* methods are the strongest candidates for investigation.
Districts flagged only by SD may be worth a secondary review but are less certain.

**Using the filters:** Select a specific indicator or year range to focus your analysis. The KPI cards and all charts update automatically.
        """)

    col_f1, col_f2, col_f3 = st.columns(3)
    with col_f1:
        indicator_sel = st.selectbox("Indicator", ["Both", "Penta 1", "Penta 3"])
    with col_f2:
        all_years  = sorted(outliers["year"].unique())
        year_range = st.select_slider("Year range", options=all_years, value=(all_years[0], all_years[-1]))
    with col_f3:
        method_sel = st.selectbox("Outlier method", ["SD (Z-score > 3)", "MAD (score > 5)"])

    df_out = outliers.copy()
    if indicator_sel != "Both":
        df_out = df_out[df_out["indicator_label"] == indicator_sel]
    df_out = df_out[(df_out["year"] >= year_range[0]) & (df_out["year"] <= year_range[1])]

    outlier_col = "outliers_sd" if "SD" in method_sel else "outliers_mad"
    overall_pct = df_out[outlier_col].sum() / max(df_out["total_entries"].sum(), 1) * 100

    # ── KPIs ──────────────────────────────────────────────────────────────────
    k1, k2, k3, k4 = st.columns(4)
    with k1:
        st.markdown(f"""<div class="metric-card">
            <h4>Total monthly reports</h4>
            <h2>{df_out['total_entries'].sum():,}</h2></div>""", unsafe_allow_html=True)
    with k2:
        st.markdown(f"""<div class="metric-card">
            <h4>SD outliers flagged</h4>
            <h2>{df_out['outliers_sd'].sum():,}</h2></div>""", unsafe_allow_html=True)
    with k3:
        st.markdown(f"""<div class="metric-card">
            <h4>MAD outliers flagged</h4>
            <h2>{df_out['outliers_mad'].sum():,}</h2></div>""", unsafe_allow_html=True)
    with k4:
        st.markdown(f"""<div class="metric-card">
            <h4>Outlier rate ({method_sel.split()[0]})</h4>
            <h2>{overall_pct:.2f}%</h2></div>""", unsafe_allow_html=True)

    # ── Trend over time ───────────────────────────────────────────────────────
    st.markdown('<div class="section-header">Outlier Rate by Year and Indicator</div>', unsafe_allow_html=True)
    chart_helper(
        "Each line shows the percentage of monthly facility reports flagged as outliers per year. "
        "Solid lines = SD method; dotted lines = MAD method. "
        "When both lines move together the trend is robust and not a methodological artefact. "
        "A U-shape (declining then rising) may indicate improving data practices followed by a "
        "new wave of reporting issues — or increased catch-up campaign activity."
    )

    trend = (df_out.groupby(["year", "indicator_label"])
             .agg(total=("total_entries", "sum"), sd=("outliers_sd", "sum"), mad=("outliers_mad", "sum"))
             .reset_index())
    trend["pct_sd"]  = trend["sd"]  / trend["total"] * 100
    trend["pct_mad"] = trend["mad"] / trend["total"] * 100

    colours = {"Penta 1": "#2874a6", "Penta 3": "#1abc9c"}
    fig_trend = go.Figure()
    for ind in trend["indicator_label"].unique():
        sub = trend[trend["indicator_label"] == ind]
        fig_trend.add_trace(go.Scatter(
            x=sub["year"], y=sub["pct_sd"], name=f"{ind} (SD)",
            mode="lines+markers", line=dict(color=colours.get(ind, "#999"), width=2),
        ))
        fig_trend.add_trace(go.Scatter(
            x=sub["year"], y=sub["pct_mad"], name=f"{ind} (MAD)",
            mode="lines+markers", line=dict(color=colours.get(ind, "#999"), width=2, dash="dot"),
        ))

    avg_by_year  = trend.groupby("year")["pct_sd"].mean()
    min_year_sd  = avg_by_year.idxmin()
    max_year_sd  = avg_by_year.idxmax()
    fig_trend.add_annotation(
        x=min_year_sd, y=avg_by_year[min_year_sd],
        text="📉 Lowest outlier rate", showarrow=True, arrowhead=2,
        ax=50, ay=-30, font=dict(size=11, color="#555"),
    )
    if max_year_sd != min_year_sd:
        fig_trend.add_annotation(
            x=max_year_sd, y=avg_by_year[max_year_sd],
            text="📈 Review data quality", showarrow=True, arrowhead=2,
            ax=-60, ay=-30, font=dict(size=11, color="#c0392b"),
        )
    fig_trend.update_layout(
        xaxis_title="Year", yaxis_title="% of reports flagged as outliers",
        height=420, paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
        yaxis=dict(gridcolor="#dde6f0"),
        legend=dict(orientation="h", yanchor="bottom", y=1.01),
        margin=dict(l=10, r=10, t=20, b=40),
    )
    st.plotly_chart(fig_trend, use_container_width=True)

    trend_dir = "increased" if avg_by_year.iloc[-1] > avg_by_year.iloc[-3] else "decreased or stabilised"
    insight_box("Outlier Trend — What this means", [
        f"Outlier rates have {trend_dir} over the most recent 3 years of the selected filter.",
        "SD and MAD lines moving in the same direction confirms the trend is real, not a methodological artefact.",
        "A sustained post-2022 rise warrants investigation into HMIS reporting practices — particularly in high-outlier districts shown below.",
    ])

    # ── Distribution histograms ───────────────────────────────────────────────
    st.markdown('<div class="section-header">Outlier Rate Distribution Across Districts</div>', unsafe_allow_html=True)
    chart_helper(
        "Each bar represents a count of districts with that average outlier rate. "
        "Most districts should cluster at the left (low rates). "
        "A long tail to the right means a small number of districts are driving most of the quality issues — "
        "those are the priority districts for targeted data review."
    )

    admin2_summary = (df_out.groupby(["admin2", "indicator_label"])
                      .agg(pct_sd=("pct_sd", "mean"), pct_mad=("pct_mad", "mean"))
                      .reset_index())
    high_sd_n = (admin2_summary.groupby("admin2")["pct_sd"].mean() > 2).sum()

    tab1, tab2 = st.tabs(["SD Method", "MAD Method"])
    with tab1:
        fig_h = px.histogram(
            admin2_summary, x="pct_sd", color="indicator_label", nbins=30,
            labels={"pct_sd": "Average % SD outliers per district", "indicator_label": "Indicator"},
            color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
            barmode="overlay", opacity=0.75,
        )
        fig_h.update_layout(height=330, paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
                             margin=dict(l=10, r=10, t=10, b=40))
        st.plotly_chart(fig_h, use_container_width=True)
    with tab2:
        fig_h2 = px.histogram(
            admin2_summary, x="pct_mad", color="indicator_label", nbins=30,
            labels={"pct_mad": "Average % MAD outliers per district", "indicator_label": "Indicator"},
            color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
            barmode="overlay", opacity=0.75,
        )
        fig_h2.update_layout(height=330, paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
                              margin=dict(l=10, r=10, t=10, b=40))
        st.plotly_chart(fig_h2, use_container_width=True)

    insight_box("Distribution — What to look for", [
        f"{high_sd_n} district(s) have an average SD outlier rate above 2% — these are the tail of the distribution and highest priority for review.",
        "The MAD distribution typically sits lower and tighter than SD — expected, as MAD is the more conservative method.",
        "If Penta 1 and Penta 3 histograms look similar, data quality issues are not vaccine-specific — they likely reflect general facility reporting challenges.",
    ])

    # ── Scatter: SD vs MAD ────────────────────────────────────────────────────
    st.markdown('<div class="section-header">SD vs MAD Agreement per District</div>', unsafe_allow_html=True)
    chart_helper(
        "Each dot is one district. The dashed diagonal is the line of equality — dots on this line "
        "were flagged at the same rate by both methods. "
        "Dots above the line: MAD flagged more than SD. Dots below: SD flagged more. "
        "Districts in the top-right corner (high on both axes) are the strongest data quality concerns."
    )

    max_val   = admin2_summary[["pct_sd", "pct_mad"]].max().max()
    both_high = admin2_summary[(admin2_summary["pct_sd"] > 2) & (admin2_summary["pct_mad"] > 2)]

    fig_sc = px.scatter(
        admin2_summary, x="pct_sd", y="pct_mad", color="indicator_label",
        labels={"pct_sd": "SD outlier rate (%)", "pct_mad": "MAD outlier rate (%)",
                "indicator_label": "Indicator"},
        color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
        opacity=0.6, hover_data={"admin2": True},
    )
    fig_sc.add_shape(type="line", x0=0, y0=0, x1=max_val, y1=max_val,
                     line=dict(dash="dash", color="grey"))
    fig_sc.add_annotation(
        x=max_val * 0.82, y=max_val * 0.96,
        text="← Both methods agree here", showarrow=False,
        font=dict(size=10, color="#888"),
    )
    fig_sc.update_layout(height=430, paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#f9fbff",
                          margin=dict(l=10, r=10, t=10, b=40))
    st.plotly_chart(fig_sc, use_container_width=True)

    insight_box("SD vs MAD Agreement — Interpretation", [
        f"{len(both_high)} district-indicator combinations are flagged as high-outlier (>2%) by both methods — these are the most credible data quality concerns.",
        "Most dots cluster below the diagonal, meaning SD flags more outliers than MAD. This is normal — SD is the more sensitive method.",
        "Points far above the diagonal (MAD >> SD) may indicate a facility with a skewed baseline where the mean is being inflated by a few large historical values.",
    ])

    with st.expander("📋 View filtered outlier data"):
        display = df_out[["admin2", "indicator_label", "year", "total_entries",
                           "outliers_sd", "outliers_mad", "pct_sd", "pct_mad"]].copy()
        display.columns = ["District (Admin2)", "Indicator", "Year", "Total Reports",
                            "SD Outliers", "MAD Outliers", "SD %", "MAD %"]
        display["SD %"]  = display["SD %"].round(2)
        display["MAD %"] = display["MAD %"].round(2)
        st.dataframe(display.sort_values(["Year", "District (Admin2)"]),
                     use_container_width=True, hide_index=True)

    glossary_expander()