# Carrega bibliotecas
library(tidyverse)
library(scales)
library(gridExtra)
library(viridis)

# Define parâmetros dos canais
criar_parametros_canais <- function() {
  tibble(
    canal = c('Google Ads', 'Facebook Ads', 'Instagram Ads', 'Email Marketing', 'LinkedIn Ads'),
    cpc_medio = c(2.5, 1.8, 1.5, 0.05, 5.0),
    cpc_desvio = c(0.5, 0.4, 0.3, 0.01, 1.0),
    ctr_medio = c(0.035, 0.025, 0.022, 0.15, 0.028),
    ctr_desvio = c(0.008, 0.006, 0.005, 0.03, 0.007),
    conversao_media = c(0.03, 0.025, 0.02, 0.05, 0.04),
    conversao_desvio = c(0.008, 0.007, 0.005, 0.015, 0.01),
    ticket_medio = c(150, 120, 100, 200, 300),
    ticket_desvio = c(30, 25, 20, 50, 80)
  )
}

simular_canal <- function(nome_canal, budget, parametros, n_simulacoes = 10000) {
  # Filtra parâmetros do canal
  canal <- parametros |>
    filter(canal == nome_canal) |>
    slice(1)
  
  # Vetoriza as simulações para melhor performance
  simulacoes <- tibble(
    simulacao = 1:n_simulacoes,
    # Simula variáveis com distribuição normal truncada
    cpc = pmax(0.01, rnorm(n_simulacoes, canal$cpc_medio, canal$cpc_desvio)),
    ctr = pmin(pmax(rnorm(n_simulacoes, canal$ctr_medio, canal$ctr_desvio), 0.001), 1),
    taxa_conversao = pmin(pmax(rnorm(n_simulacoes, canal$conversao_media, canal$conversao_desvio), 0.001), 1),
    ticket_medio = pmax(10, rnorm(n_simulacoes, canal$ticket_medio, canal$ticket_desvio))
  ) |>
    mutate(
      cliques = as.integer(budget / cpc),
      impressoes = as.integer(cliques / ctr),
      conversoes = as.integer(cliques * taxa_conversao),
      receita = conversoes * ticket_medio,
      lucro = receita - budget,
      roi = if_else(budget > 0, (lucro / budget) * 100, 0),
      canal = nome_canal
    )
  
  return(simulacoes)
}

analisar_resultados_canal <- function(df_simulacao, canal_nome) {
  # Calcula estatísticas
  stats <- df_simulacao |>
    summarise(
      roi_medio = mean(roi),
      roi_mediano = median(roi),
      roi_desvio = sd(roi),
      prob_prejuizo = mean(lucro < 0) * 100,
      var_5pct = quantile(lucro, 0.05),
      lucro_esperado = mean(lucro)
    )
  
  # Gráfico 1: Distribuição do ROI
  p1 <- ggplot(df_simulacao, aes(x = roi)) +
    geom_histogram(bins = 50, fill = "blue", alpha = 0.7, color = "black") +
    geom_vline(xintercept = stats$roi_medio, color = "red", 
               linetype = "dashed", size = 1) +
    labs(
      title = "Distribuição do ROI",
      x = "ROI (%)",
      y = "Frequência"
    ) +
    annotate("text", x = stats$roi_medio + 10, y = Inf, 
             label = sprintf("Média: %.1f%%", stats$roi_medio),
             vjust = 2, hjust = 0, color = "red")
  
  # Gráfico 2: Distribuição do Lucro
  p2 <- ggplot(df_simulacao, aes(x = lucro)) +
    geom_histogram(bins = 50, fill = "green", alpha = 0.7, color = "black") +
    geom_vline(xintercept = stats$lucro_esperado, color = "red", 
               linetype = "dashed", size = 1) +
    labs(
      title = "Distribuição do Lucro",
      x = "Lucro (R$)",
      y = "Frequência"
    ) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "R$ "))
  
  # Gráfico 3: Análise de Risco
  risco_data <- tibble(
    categoria = c("Prejuízo", "Lucro"),
    probabilidade = c(stats$prob_prejuizo, 100 - stats$prob_prejuizo)
  )
  
  p3 <- ggplot(risco_data, aes(x = categoria, y = probabilidade, fill = categoria)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", probabilidade)), 
              vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Prejuízo" = "red", "Lucro" = "green")) +
    labs(
      title = "Análise de Risco",
      y = "Probabilidade (%)"
    ) +
    theme(legend.position = "none")
  
  # Gráfico 4: Boxplot de métricas
  metricas_long <- df_simulacao |>
    select(conversoes, receita, lucro) |>
    mutate(
      receita = receita / 100,
      lucro = lucro / 100
    ) |>
    pivot_longer(everything(), names_to = "metrica", values_to = "valor")
  
  p4 <- ggplot(metricas_long, aes(x = metrica, y = valor)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    labs(
      title = "Variabilidade das Métricas",
      y = "Valor (Receita e Lucro /100)"
    )
  
  # Combina os gráficos
  grid.arrange(p1, p2, p3, p4, ncol = 2, 
               top = paste("Análise de Monte Carlo -", canal_nome))
  
  # Imprime estatísticas
  cat("\n=== Estatísticas para", canal_nome, "===\n")
  cat("ROI Médio:", sprintf("%.2f%%", stats$roi_medio), "\n")
  cat("ROI Mediano:", sprintf("%.2f%%", stats$roi_mediano), "\n")
  cat("Desvio Padrão ROI:", sprintf("%.2f%%", stats$roi_desvio), "\n")
  cat("Probabilidade de Prejuízo:", sprintf("%.1f%%", stats$prob_prejuizo), "\n")
  cat("Value at Risk (5%):", sprintf("R$ %.2f", stats$var_5pct), "\n")
  cat("Lucro Esperado:", sprintf("R$ %.2f", stats$lucro_esperado), "\n")
}

otimizar_portfolio <- function(budget_total, parametros, n_simulacoes = 5000) {
  canais <- parametros$canal
  n_canais <- length(canais)
  
  # Gera alocações usando Dirichlet
  n_portfolios <- 1000
  
  # Função para gerar uma alocação Dirichlet
  gerar_alocacao <- function() {
    x <- rgamma(n_canais, 1)
    x / sum(x)
  }
  
  # Gera múltiplas alocações
  alocacoes <- replicate(n_portfolios, gerar_alocacao()) |> t()
  
  # Calcula métricas para cada portfolio
  resultados_portfolio <- 1:n_portfolios |>
    map_dfr(function(i) {
      alocacao <- alocacoes[i, ]
      
      # Simula cada canal
      metricas <- 1:n_canais |>
        map_dfr(function(j) {
          budget_canal <- budget_total * alocacao[j]
          if (budget_canal > 0) {
            sim <- simular_canal(canais[j], budget_canal, parametros, 1000)
            tibble(
              canal = canais[j],
              peso = alocacao[j],
              roi_medio = mean(sim$roi),
              lucro_medio = mean(sim$lucro),
              risco = sd(sim$lucro)
            )
          } else {
            tibble(
              canal = canais[j],
              peso = 0,
              roi_medio = 0,
              lucro_medio = 0,
              risco = 0
            )
          }
        })
      
      # Calcula métricas do portfolio
      tibble(
        portfolio = i,
        roi_esperado = sum(metricas$roi_medio * metricas$peso),
        lucro_esperado = sum(metricas$lucro_medio),
        risco = sqrt(sum((metricas$risco * metricas$peso)^2)),
        sharpe_ratio = lucro_esperado / risco,
        alocacao = list(alocacao)
      )
    })
  
  # Encontra portfolios ótimos
  melhor_roi <- resultados_portfolio |>
    slice_max(roi_esperado, n = 1)
  
  melhor_sharpe <- resultados_portfolio |>
    slice_max(sharpe_ratio, n = 1)
  
  return(list(
    portfolios = resultados_portfolio,
    melhor_roi = melhor_roi,
    melhor_sharpe = melhor_sharpe
  ))
}

plotar_fronteira_eficiente <- function(resultado_otimizacao) {
  portfolios <- resultado_otimizacao$portfolios
  melhor_roi <- resultado_otimizacao$melhor_roi
  melhor_sharpe <- resultado_otimizacao$melhor_sharpe
  
  p <- ggplot(portfolios, aes(x = risco, y = lucro_esperado)) +
    geom_point(aes(color = sharpe_ratio), size = 3, alpha = 0.6) +
    scale_color_viridis(name = "Sharpe Ratio") +
    # Destaca portfolios ótimos
    geom_point(data = melhor_roi, 
               color = "red", size = 8, shape = 17) +
    geom_point(data = melhor_sharpe, 
               color = "orange", size = 8, shape = 17) +
    # Adiciona labels
    annotate("text", x = melhor_roi$risco, y = melhor_roi$lucro_esperado,
             label = "Melhor ROI", vjust = -1, color = "red", fontface = "bold") +
    annotate("text", x = melhor_sharpe$risco, y = melhor_sharpe$lucro_esperado,
             label = "Melhor Sharpe", vjust = -1, color = "orange", fontface = "bold") +
    labs(
      title = "Fronteira Eficiente - Portfolios de Marketing",
      x = "Risco (Desvio Padrão do Lucro)",
      y = "Lucro Esperado (R$)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right"
    )
  
  print(p)
}

exibir_alocacao_otima <- function(melhor_portfolio, parametros, titulo) {
  canais <- parametros$canal
  alocacao <- melhor_portfolio$alocacao[[1]]
  
  # Cria data frame para o gráfico
  dados_pizza <- tibble(
    canal = canais,
    alocacao = alocacao * 100
  ) |>
    filter(alocacao > 1) |>  # Filtra canais com alocação > 1%
    arrange(desc(alocacao))
  
  # Gráfico de pizza
  p <- ggplot(dados_pizza, aes(x = "", y = alocacao, fill = canal)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = sprintf("%.1f%%", alocacao)),
              position = position_stack(vjust = 0.5)) +
    labs(
      title = paste0(titulo, "\n",
                    sprintf("ROI Esperado: %.1f%% | Lucro Esperado: R$ %.2f",
                           melhor_portfolio$roi_esperado,
                           melhor_portfolio$lucro_esperado)),
      fill = "Canal"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "right"
    )
  
  print(p)
}

# Exemplo de uso completo
# Define parâmetros
parametros <- criar_parametros_canais()
budget_total <- 50000  # R$ 50.000

# 1. Análise individual de cada canal
cat("=== ANÁLISE INDIVIDUAL DOS CANAIS ===\n")
for (canal_nome in parametros$canal) {
  cat("\nSimulando", canal_nome, "...\n")
  simulacao <- simular_canal(canal_nome, budget_total/5, parametros, n_simulacoes = 10000)
  analisar_resultados_canal(simulacao, canal_nome)
}

# 2. Otimização de portfolio
cat("\n=== OTIMIZAÇÃO DE PORTFOLIO ===\n")
resultado_otimizacao <- otimizar_portfolio(budget_total, parametros)

# 3. Visualização da fronteira eficiente
plotar_fronteira_eficiente(resultado_otimizacao)

# 4. Exibição das alocações ótimas
exibir_alocacao_otima(resultado_otimizacao$melhor_roi, parametros, "Portfolio de Melhor ROI")
exibir_alocacao_otima(resultado_otimizacao$melhor_sharpe, parametros, "Portfolio de Melhor Sharpe Ratio")

# 5. Comparação de estratégias
cat("\n=== COMPARAÇÃO DE ESTRATÉGIAS ===\n")
cat("Portfolio de Melhor ROI:\n")
cat(sprintf("  - ROI Esperado: %.2f%%\n", resultado_otimizacao$melhor_roi$roi_esperado))
cat(sprintf("  - Lucro Esperado: R$ %.2f\n", resultado_otimizacao$melhor_roi$lucro_esperado))
cat(sprintf("  - Risco: R$ %.2f\n", resultado_otimizacao$melhor_roi$risco))

cat("\nPortfolio de Melhor Sharpe Ratio:\n")
cat(sprintf("  - ROI Esperado: %.2f%%\n", resultado_otimizacao$melhor_sharpe$roi_esperado))
cat(sprintf("  - Lucro Esperado: R$ %.2f\n", resultado_otimizacao$melhor_sharpe$lucro_esperado))
cat(sprintf("  - Risco: R$ %.2f\n", resultado_otimizacao$melhor_sharpe$risco))