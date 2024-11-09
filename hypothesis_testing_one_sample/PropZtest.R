{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": []
    },
    "kernelspec": {
      "name": "ir",
      "display_name": "R"
    },
    "language_info": {
      "name": "R"
    }
  },
  "cells": [
    {
      "cell_type": "markdown",
      "source": [
        "# <center>Hypothesis Test Function for Proportion</center>"
      ],
      "metadata": {
        "id": "cBF8EHOg-DiX"
      }
    },
    {
      "cell_type": "code",
      "source": [
        "hypothesis_test_proportion <- function(x, n, p_null, alpha, tails = \"two\", decimal_places = 2) {\n",
        "\n",
        "  # Calculate the sample proportion (p_hat)\n",
        "  p_hat <- x / n\n",
        "\n",
        "  # Calculate the standard error (se)\n",
        "  se <- sqrt(p_null * (1 - p_null) / n)\n",
        "\n",
        "  # Calculate the test statistic (z-score)\n",
        "  z <- (p_hat - p_null) / se\n",
        "\n",
        "  # Calculate the critical values and p-value based on the tail\n",
        "  if (tails == \"left\") {\n",
        "    crit_val <- -abs(qnorm(alpha, mean = 0, sd = 1))\n",
        "    p_val <- pnorm(z)\n",
        "    conclusion <- ifelse(z < crit_val, \"Reject the null hypothesis\", \"Do not reject the null hypothesis\")\n",
        "  } else if (tails == \"right\") {\n",
        "    crit_val <- abs(qnorm(alpha, mean = 0, sd = 1))\n",
        "    p_val <- 1 - pnorm(z)\n",
        "    conclusion <- ifelse(z > crit_val, \"Reject the null hypothesis\", \"Do not reject the null hypothesis\")\n",
        "  } else {\n",
        "    crit_val_left <- -abs(qnorm(alpha / 2, mean = 0, sd = 1))\n",
        "    crit_val_right <- abs(qnorm(alpha / 2, mean = 0, sd = 1))\n",
        "    p_val <- 2 * (1 - pnorm(abs(z)))\n",
        "    conclusion <- ifelse(abs(z) > crit_val_right || abs(z) < crit_val_left, \"Reject the null hypothesis\", \"Do not reject the null hypothesis\")\n",
        "  }\n",
        "\n",
        "  # Round the results to the specified number of decimal places\n",
        "  z <- round(z, digits = decimal_places)\n",
        "  p_val <- round(p_val, digits = decimal_places)\n",
        "\n",
        "  if (tails == \"two\") {\n",
        "    crit_val_left <- round(crit_val_left, digits = decimal_places)\n",
        "    crit_val_right <- round(crit_val_right, digits = decimal_places)\n",
        "    cat(\"Critical values: \", crit_val_left, \", \", crit_val_right, \"\\n\")\n",
        "  } else {\n",
        "    crit_val <- round(crit_val, digits = decimal_places)\n",
        "    cat(\"Critical value: \", crit_val, \"\\n\")\n",
        "  }\n",
        "\n",
        "  cat(\"Test statistic: \", z, \"\\n\")\n",
        "  cat(\"P-value: \", p_val, \"\\n\")\n",
        "  cat(\"Conclusion: \", conclusion, \"\\n\")\n",
        "}"
      ],
      "metadata": {
        "id": "HfffhIOZ-bcu"
      },
      "execution_count": null,
      "outputs": []
    },
    {
      "cell_type": "code",
      "source": [
        "# \"left\" for one-tailed left, \"right\" for one-tailed right, \"two\" for two-tailed\n",
        "hypothesis_test_proportion(x=297, n=480, p_null=.56, alpha=0.05, tails=\"left\", decimal_places=4)"
      ],
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/"
        },
        "id": "-0QbxgEw-ShB",
        "outputId": "1a5f926b-cb36-4672-ba88-caa37121238f"
      },
      "execution_count": null,
      "outputs": [
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "Critical value:  -1.6449 \n",
            "Test statistic:  2.593 \n",
            "P-value:  0.9952 \n",
            "Conclusion:  Do not reject the null hypothesis \n"
          ]
        }
      ]
    }
  ]
}
