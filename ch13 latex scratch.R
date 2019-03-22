
\\\mu_i \sim \alpha_{GROUP[i]} \> + \> \beta_{GROUP[i]} x_i

$$
  \begin{equation*}
\left[
  \begin{array}{ccc}
  \alpha_{group} \\
  \beta_{group}\\
  \end{array} \right]
\sim MVNormal \bigg(
  \left[
    \begin{array}{ccc}
    \alpha \\
    \beta \\
    \end{array} \right],
  \textbf{S} \bigg)
\end{equation*}


\begin{equation*}
\textbf{S} =
  \left[
    \begin{array}{ccc}
    \sigma_a & 0 \\
    0 & \sigma_b\\
    \end{array}
    \right]
\textbf{R}
\left[
  \begin{array}{ccc}
  \sigma_a & 0 \\
  0 & \sigma_b\\
  \end{array}
  \right]
\end{equation*}
$$
  
  $$
  \\y_i \sim  Normal(\mu_i, \sigma)
\\\mu_i \sim \alpha_{GROUP[i]} \> + \> \beta_{GROUP[i]} x_i



\\\begin{equation*}
\left[
  \begin{array}{ccc}
  \alpha_{GROUP} \\
  \beta_{GROUP}\\
  \end{array} \right]
\sim MVNormal \bigg(
  \left[
    \begin{array}{ccc}
    \alpha \\
    \beta \\
    \end{array} \right],
  \textbf{S} \bigg)
\end{equation*}

\\begin{equation*}
\textbf{S} =
  \left[
    \begin{array}{ccc}
    \sigma_a & 0 \\
    0 & \sigma_b\\
    \end{array}
    \right]
\textbf{R}
\left[
  \begin{array}{ccc}
  \sigma_a & 0 \\
  0 & \sigma_b\\
  \end{array}
  \right]
\end{equation*}

\\\alpha \sim Normal(0, 10)
\\\beta \sim Normal(0, 1)
\\\sigma \sim HalfCauchy(0, 2)
\\(\sigma_a, \sigma_b) \sim HalfCauchy(0, 2)
$$