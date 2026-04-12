import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import json
import geopandas as gpd

# ── Page config ───────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="Tanzania Vaccine Coverage Dashboard",
    page_icon="💉",
    layout="wide",
    initial_sidebar_state="expanded",
)

# ── Custom CSS ─────────────────────────────────────────────────────────────────
st.markdown(
    """
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
</style>
""",
    unsafe_allow_html=True,
)


# ── Data loading ───────────────────────────────────────────────────────────────
@st.cache_data
def load_coverage():
    p1 = pd.read_excel("DataFiles/task1_coverage.xlsx", sheet_name="Penta1Cov")
    p3 = pd.read_excel("DataFiles/task1_coverage.xlsx", sheet_name="Penta3Cov")
    # Fix double-multiplication bug in R script (values were x100 twice)
    for df in [p1, p3]:
        for col in ["Coverage", "95% CI Lower", "95% CI Upper", "CI_Lower", "CI_Upper"]:
            if col in df.columns:
                df[col] = df[col] / 100
    # Normalise column names
    p1 = p1.rename(columns={"95% CI Lower": "CI_Lower", "95% CI Upper": "CI_Upper"})
    p3 = p3.rename(columns={"95% CI Lower": "CI_Lower", "95% CI Upper": "CI_Upper"})
    # Title-case region so it matches the GeoJSON
    for df in [p1, p3]:
        df["Region"] = df["Region"].str.title()
    # Name mapping: coverage → geojson
    name_map = {
        "Dar Es Salaam": "Dar-es-salaam",
        "Kaskazini Pemba": "Kaskazini Pemba",
        "Kaskazini Unguja": "Kaskazini Unguja",
        "Kusini Pemba": "Kusini Pemba",
        "Kusini Unguja": "Kusini Unguja",
        "Mjini Magharibi": "Mjini Magharibi",
    }
    for df in [p1, p3]:
        df["Region"] = df["Region"].replace(name_map)
    return p1, p3


@st.cache_data
def load_outliers():
    df = pd.read_excel("DataFiles/task2_outlier_summary.xlsx", sheet_name="Outlier Summary")
    df.columns = [
        "admin2",
        "indicator_type",
        "year",
        "total_entries",
        "outliers_sd",
        "outliers_mad",
        "pct_sd",
        "pct_mad",
        "pct_sd2",
        "pct_mad2",
    ]
    df["indicator_label"] = df["indicator_type"].map(
        {"penta1_u1": "Penta 1", "penta3_u1": "Penta 3"}
    )
    return df


@st.cache_data
def load_geo():
    gdf = gpd.read_file("DataFiles/tza_adm2_map.geojson")
    adm1 = gdf.dissolve(by="ADM1_EN").reset_index()[["ADM1_EN", "geometry"]]
    return adm1


penta1, penta3 = load_coverage()
outliers = load_outliers()
adm1_geo = load_geo()

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

# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 1 – Coverage Overview
# ═══════════════════════════════════════════════════════════════════════════════
if page == "📊 Coverage Overview":
    st.markdown(
        '<p class="main-title">Tanzania Vaccine Coverage Dashboard</p>',
        unsafe_allow_html=True,
    )
    st.markdown(
        '<p class="sub-title">Survey-weighted Penta 1 & Penta 3 coverage by subnational region · Children 12–23 months</p>',
        unsafe_allow_html=True,
    )

    # ── KPI row ──────────────────────────────────────────────────────────────
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.markdown(
            f"""<div class="metric-card">
            <h4>Penta 1 — National Avg</h4>
            <h2>{penta1['Coverage'].mean():.1f}%</h2></div>""",
            unsafe_allow_html=True,
        )
    with col2:
        st.markdown(
            f"""<div class="metric-card">
            <h4>Penta 3 — National Avg</h4>
            <h2>{penta3['Coverage'].mean():.1f}%</h2></div>""",
            unsafe_allow_html=True,
        )
    with col3:
        drop = penta1["Coverage"].mean() - penta3["Coverage"].mean()
        st.markdown(
            f"""<div class="metric-card">
            <h4>Penta 1→3 Dropout</h4>
            <h2>{drop:.1f} pp</h2></div>""",
            unsafe_allow_html=True,
        )
    with col4:
        n_low = (penta3["Coverage"] < 70).sum()
        st.markdown(
            f"""<div class="metric-card">
            <h4>Regions &lt; 70% Penta 3</h4>
            <h2>{n_low} / {len(penta3)}</h2></div>""",
            unsafe_allow_html=True,
        )

    # ── Vaccine selector ─────────────────────────────────────────────────────
    st.markdown(
        '<div class="section-header">Coverage by Region with 95% CI</div>',
        unsafe_allow_html=True,
    )
    vaccine = st.selectbox("Select indicator", ["Penta 1", "Penta 3"])
    df_sel = penta1.copy() if vaccine == "Penta 1" else penta3.copy()
    df_sel = df_sel.sort_values("Coverage", ascending=True)

    fig = go.Figure()
    fig.add_trace(
        go.Bar(
            y=df_sel["Region"],
            x=df_sel["Coverage"],
            orientation="h",
            marker_color=px.colors.sequential.Blues_r[2],
            error_x=dict(
                type="data",
                symmetric=False,
                array=df_sel["CI_Upper"] - df_sel["Coverage"],
                arrayminus=df_sel["Coverage"] - df_sel["CI_Lower"],
                color="#444",
            ),
            hovertemplate=(
                "<b>%{y}</b><br>"
                f"{vaccine} coverage: %{{x:.1f}}%<br>"
                "95% CI: %{customdata[0]:.1f}% – %{customdata[1]:.1f}%<extra></extra>"
            ),
            customdata=list(zip(df_sel["CI_Lower"], df_sel["CI_Upper"])),
        )
    )
    fig.add_vline(
        x=90,
        line_dash="dash",
        line_color="red",
        annotation_text="90% target",
        annotation_position="top right",
    )
    fig.update_layout(
        xaxis_title="Coverage (%)",
        yaxis_title="",
        height=700,
        margin=dict(l=10, r=30, t=10, b=40),
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="#f9fbff",
        xaxis=dict(range=[0, 105], gridcolor="#dde6f0"),
    )
    st.plotly_chart(fig, use_container_width=True)

    # ── Side-by-side comparison ───────────────────────────────────────────────
    st.markdown(
        '<div class="section-header">Penta 1 vs Penta 3 Comparison</div>',
        unsafe_allow_html=True,
    )
    merged = (
        penta1[["Region", "Coverage"]]
        .rename(columns={"Coverage": "Penta1"})
        .merge(
            penta3[["Region", "Coverage"]].rename(columns={"Coverage": "Penta3"}),
            on="Region",
        )
        .sort_values("Penta3")
    )
    merged["Dropout"] = merged["Penta1"] - merged["Penta3"]

    fig2 = go.Figure()
    fig2.add_trace(
        go.Bar(
            name="Penta 1",
            y=merged["Region"],
            x=merged["Penta1"],
            orientation="h",
            marker_color="#2874a6",
        )
    )
    fig2.add_trace(
        go.Bar(
            name="Penta 3",
            y=merged["Region"],
            x=merged["Penta3"],
            orientation="h",
            marker_color="#a9cce3",
        )
    )
    fig2.update_layout(
        barmode="overlay",
        height=700,
        xaxis_title="Coverage (%)",
        xaxis=dict(range=[0, 105], gridcolor="#dde6f0"),
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="#f9fbff",
        legend=dict(orientation="h", yanchor="bottom", y=1.01, xanchor="right", x=1),
        margin=dict(l=10, r=30, t=10, b=40),
    )
    fig2.add_vline(x=90, line_dash="dash", line_color="red")
    st.plotly_chart(fig2, use_container_width=True)

    # ── Data table ───────────────────────────────────────────────────────────
    with st.expander("📋 View raw data table"):
        combined = penta1[["Region", "Coverage", "CI_Lower", "CI_Upper"]].copy()
        combined.columns = ["Region", "Penta1 (%)", "P1 CI Lower", "P1 CI Upper"]
        combined = combined.merge(
            penta3[["Region", "Coverage", "CI_Lower", "CI_Upper"]].rename(
                columns={
                    "Coverage": "Penta3 (%)",
                    "CI_Lower": "P3 CI Lower",
                    "CI_Upper": "P3 CI Upper",
                }
            ),
            on="Region",
        )
        combined["Dropout (pp)"] = (
            combined["Penta1 (%)"] - combined["Penta3 (%)"]
        ).round(1)
        for col in [
            "Penta1 (%)",
            "P1 CI Lower",
            "P1 CI Upper",
            "Penta3 (%)",
            "P3 CI Lower",
            "P3 CI Upper",
        ]:
            combined[col] = combined[col].round(1)
        st.dataframe(
            combined.sort_values("Region"), use_container_width=True, hide_index=True
        )


# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 2 – Choropleth Map
# ═══════════════════════════════════════════════════════════════════════════════
elif page == "🗺️ Choropleth Map":
    st.markdown(
        '<p class="main-title">Vaccine Coverage Map — Tanzania</p>',
        unsafe_allow_html=True,
    )
    st.markdown(
        '<p class="sub-title">Survey-weighted coverage merged with ADM1 boundary data</p>',
        unsafe_allow_html=True,
    )

    map_vaccine = st.selectbox("Select indicator", ["Penta 1", "Penta 3"], key="map_v")
    df_map = penta1 if map_vaccine == "Penta 1" else penta3

    # Merge geo + coverage
    geo_merged = adm1_geo.merge(
        df_map[["Region", "Coverage", "CI_Lower", "CI_Upper"]],
        left_on="ADM1_EN",
        right_on="Region",
        how="left",
    )

    geojson_data = json.loads(geo_merged.to_json())

    fig_map = px.choropleth(
        geo_merged,
        geojson=geojson_data,
        locations=geo_merged.index,
        color="Coverage",
        hover_name="ADM1_EN",
        hover_data={"Coverage": ":.1f", "CI_Lower": ":.1f", "CI_Upper": ":.1f"},
        color_continuous_scale=[
            [0.0, "#d73027"],
            [0.4, "#f46d43"],
            [0.55, "#fee090"],
            [0.7, "#abd9e9"],
            [0.85, "#74add1"],
            [1.0, "#1a5276"],
        ],
        labels={"Coverage": "Coverage (%)"},
        range_color=[50, 100],
    )
    fig_map.update_geos(fitbounds="locations", visible=False)
    fig_map.update_layout(
        margin=dict(l=0, r=0, t=0, b=0),
        height=580,
        paper_bgcolor="rgba(0,0,0,0)",
        coloraxis_colorbar=dict(title="Coverage (%)", ticksuffix="%"),
    )
    st.plotly_chart(fig_map, use_container_width=True)

    # Legend / notes
    st.info(
        "**Note:** Coverage values are survey-weighted estimates from DHS data for children aged 12–23 months. "
        "Values above 90% meet the WHO immunisation target (dashed line). "
        "Grey regions indicate no data match between survey and boundary files."
    )

    # Unmatched warning
    unmatched = geo_merged[geo_merged["Coverage"].isna()]["ADM1_EN"].tolist()
    if unmatched:
        st.warning(f"Regions without matched coverage data: {', '.join(unmatched)}")


# ═══════════════════════════════════════════════════════════════════════════════
# PAGE 3 – Outlier Analysis
# ═══════════════════════════════════════════════════════════════════════════════
elif page == "🔍 Outlier Analysis":
    st.markdown(
        '<p class="main-title">HMIS Data Quality — Outlier Assessment</p>',
        unsafe_allow_html=True,
    )
    st.markdown(
        '<p class="sub-title">Administrative (routine HMIS) data · Outliers detected via SD (Z-score &gt;3) and MAD (score &gt;5)</p>',
        unsafe_allow_html=True,
    )

    col_f1, col_f2, col_f3 = st.columns(3)
    with col_f1:
        indicator_sel = st.selectbox("Indicator", ["Both", "Penta 1", "Penta 3"])
    with col_f2:
        all_years = sorted(outliers["year"].unique())
        year_range = st.select_slider(
            "Year range", options=all_years, value=(all_years[0], all_years[-1])
        )
    with col_f3:
        method_sel = st.selectbox(
            "Outlier method", ["SD (Z-score > 3)", "MAD (score > 5)", "Either"]
        )

    # Filter
    df_out = outliers.copy()
    if indicator_sel != "Both":
        df_out = df_out[df_out["indicator_label"] == indicator_sel]
    df_out = df_out[
        (df_out["year"] >= year_range[0]) & (df_out["year"] <= year_range[1])
    ]

    pct_col = {
        "SD (Z-score > 3)": "pct_sd",
        "MAD (score > 5)": "pct_mad",
        "Either": "pct_sd",
    }[method_sel]

    # ── KPIs ─────────────────────────────────────────────────────────────────
    k1, k2, k3, k4 = st.columns(4)
    with k1:
        st.markdown(
            f"""<div class="metric-card">
            <h4>Total data points</h4>
            <h2>{df_out['total_entries'].sum():,}</h2></div>""",
            unsafe_allow_html=True,
        )
    with k2:
        st.markdown(
            f"""<div class="metric-card">
            <h4>SD outliers</h4>
            <h2>{df_out['outliers_sd'].sum():,}</h2></div>""",
            unsafe_allow_html=True,
        )
    with k3:
        st.markdown(
            f"""<div class="metric-card">
            <h4>MAD outliers</h4>
            <h2>{df_out['outliers_mad'].sum():,}</h2></div>""",
            unsafe_allow_html=True,
        )
    with k4:
        overall_pct = (
            df_out["outliers_sd"].sum() / max(df_out["total_entries"].sum(), 1) * 100
        )
        st.markdown(
            f"""<div class="metric-card">
            <h4>Overall outlier rate (SD)</h4>
            <h2>{overall_pct:.2f}%</h2></div>""",
            unsafe_allow_html=True,
        )

    # ── Trend over time ───────────────────────────────────────────────────────
    st.markdown(
        '<div class="section-header">Outlier Rate by Year and Indicator</div>',
        unsafe_allow_html=True,
    )

    trend = (
        df_out.groupby(["year", "indicator_label"])
        .agg(
            total=("total_entries", "sum"),
            sd=("outliers_sd", "sum"),
            mad=("outliers_mad", "sum"),
        )
        .reset_index()
    )
    trend["pct_sd"] = trend["sd"] / trend["total"] * 100
    trend["pct_mad"] = trend["mad"] / trend["total"] * 100

    fig_trend = go.Figure()
    colours = {"Penta 1": "#2874a6", "Penta 3": "#1abc9c"}
    for ind in trend["indicator_label"].unique():
        sub = trend[trend["indicator_label"] == ind]
        fig_trend.add_trace(
            go.Scatter(
                x=sub["year"],
                y=sub["pct_sd"],
                name=f"{ind} (SD)",
                mode="lines+markers",
                line=dict(color=colours.get(ind, "#999"), width=2),
            )
        )
        fig_trend.add_trace(
            go.Scatter(
                x=sub["year"],
                y=sub["pct_mad"],
                name=f"{ind} (MAD)",
                mode="lines+markers",
                line=dict(color=colours.get(ind, "#999"), width=2, dash="dot"),
            )
        )
    fig_trend.update_layout(
        xaxis_title="Year",
        yaxis_title="% Outliers",
        height=380,
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="#f9fbff",
        yaxis=dict(gridcolor="#dde6f0"),
        legend=dict(orientation="h", yanchor="bottom", y=1.01),
        margin=dict(l=10, r=10, t=10, b=40),
    )
    st.plotly_chart(fig_trend, use_container_width=True)

    # ── Distribution by admin2 ────────────────────────────────────────────────
    st.markdown(
        '<div class="section-header">Outlier Rate Distribution Across Admin2 Units</div>',
        unsafe_allow_html=True,
    )

    admin2_summary = (
        df_out.groupby(["admin2", "indicator_label"])
        .agg(pct_sd=("pct_sd", "mean"), pct_mad=("pct_mad", "mean"))
        .reset_index()
    )

    tab1, tab2 = st.tabs(["SD Method", "MAD Method"])
    with tab1:
        fig_h = px.histogram(
            admin2_summary,
            x="pct_sd",
            color="indicator_label",
            nbins=30,
            labels={"pct_sd": "Mean % SD Outliers", "indicator_label": "Indicator"},
            color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
            barmode="overlay",
            opacity=0.75,
        )
        fig_h.update_layout(
            height=320,
            paper_bgcolor="rgba(0,0,0,0)",
            plot_bgcolor="#f9fbff",
            margin=dict(l=10, r=10, t=10, b=40),
        )
        st.plotly_chart(fig_h, use_container_width=True)
    with tab2:
        fig_h2 = px.histogram(
            admin2_summary,
            x="pct_mad",
            color="indicator_label",
            nbins=30,
            labels={"pct_mad": "Mean % MAD Outliers", "indicator_label": "Indicator"},
            color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
            barmode="overlay",
            opacity=0.75,
        )
        fig_h2.update_layout(
            height=320,
            paper_bgcolor="rgba(0,0,0,0)",
            plot_bgcolor="#f9fbff",
            margin=dict(l=10, r=10, t=10, b=40),
        )
        st.plotly_chart(fig_h2, use_container_width=True)

    # ── Scatter: SD vs MAD ───────────────────────────────────────────────────
    st.markdown(
        '<div class="section-header">SD vs MAD Outlier Rates per Admin2 Unit</div>',
        unsafe_allow_html=True,
    )
    fig_sc = px.scatter(
        admin2_summary,
        x="pct_sd",
        y="pct_mad",
        color="indicator_label",
        labels={
            "pct_sd": "SD outlier rate (%)",
            "pct_mad": "MAD outlier rate (%)",
            "indicator_label": "Indicator",
        },
        color_discrete_map={"Penta 1": "#2874a6", "Penta 3": "#1abc9c"},
        opacity=0.6,
        hover_data={"admin2": True},
    )
    fig_sc.add_shape(
        type="line",
        x0=0,
        y0=0,
        x1=admin2_summary[["pct_sd", "pct_mad"]].max().max(),
        y1=admin2_summary[["pct_sd", "pct_mad"]].max().max(),
        line=dict(dash="dash", color="grey"),
    )
    fig_sc.update_layout(
        height=400,
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="#f9fbff",
        margin=dict(l=10, r=10, t=10, b=40),
    )
    st.plotly_chart(fig_sc, use_container_width=True)

    # ── Raw data table ───────────────────────────────────────────────────────
    with st.expander("📋 View filtered outlier data"):
        display = df_out[
            [
                "admin2",
                "indicator_label",
                "year",
                "total_entries",
                "outliers_sd",
                "outliers_mad",
                "pct_sd",
                "pct_mad",
            ]
        ].copy()
        display.columns = [
            "Admin2",
            "Indicator",
            "Year",
            "Total Entries",
            "SD Outliers",
            "MAD Outliers",
            "SD %",
            "MAD %",
        ]
        display["SD %"] = display["SD %"].round(2)
        display["MAD %"] = display["MAD %"].round(2)
        st.dataframe(
            display.sort_values(["Year", "Admin2"]),
            use_container_width=True,
            hide_index=True,
        )
