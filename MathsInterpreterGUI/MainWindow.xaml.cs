using System;
using System.Windows;
using System.Windows.Controls;
using MathsInterpreterBackend;

namespace MathsInterpreterGUI
{
    public partial class MainWindow : Window
    {
        private bool IsLetterMode = false;

        public MainWindow()
        {
            InitializeComponent();
        }

        // Toggle between numbers and algebraic letters
        private void ToggleModeButton_Click(object sender, RoutedEventArgs e)
        {
            IsLetterMode = !IsLetterMode;

            if (IsLetterMode)
            {
                ToggleModeButton.Content = "1↔x";
                Num7Button.Content = "x";
                Num8Button.Content = "y";
                Num9Button.Content = "z";
                Num4Button.Content = "a";
                Num5Button.Content = "b";
                Num6Button.Content = "c";
                Num1Button.Content = "m";
                Num2Button.Content = "n";
                Num3Button.Content = "p";
                Num0Button.Content = "q";
            }
            else
            {
                ToggleModeButton.Content = "x↔1";
                Num7Button.Content = "7";
                Num8Button.Content = "8";
                Num9Button.Content = "9";
                Num4Button.Content = "4";
                Num5Button.Content = "5";
                Num6Button.Content = "6";
                Num1Button.Content = "1";
                Num2Button.Content = "2";
                Num3Button.Content = "3";
                Num0Button.Content = "0";
            }
        }

        // Append number or letter to the input
        private void NumberOrLetterButton_Click(object sender, RoutedEventArgs e)
        {
            string buttonText = (sender as Button).Content.ToString();
            ExpressionTextBox.Text += buttonText;

            // Revert to numbers after clicking a letter in letter mode
            if (IsLetterMode)
            {
                ToggleModeButton_Click(sender, e);
            }
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            // Clear the input field
            ExpressionTextBox.Text = string.Empty;

            // Clear any previous results or errors
            ResultTextBlock.Text = "";
            ErrorTextBlock.Text = "";
        }


        // General button click handler for operators and special characters
        private void Button_Click(object sender, RoutedEventArgs e)
        {
            string buttonText = (sender as Button).Content.ToString();
            ExpressionTextBox.Text += buttonText;
        }

        // Evaluate the expression using the backend
        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                DebugTextBlock.Text += "Starting calculation...\n";

                if (ExpressionTextBox.Text.Contains("="))
                {
                    // Solve equations using the backend solver
                    DebugTextBlock.Text += $"Input: {ExpressionTextBox.Text}\n";

                    var result = MathsInterpreterBackend.Solver.solveLinearEquation(ExpressionTextBox.Text);
                    ResultTextBlock.Text = $"Result: {result}";

                    DebugTextBlock.Text += $"Result: {result}\n";
                }
                else
                {
                    // Interpret expressions using the backend parser
                    DebugTextBlock.Text += $"Parsing input: {ExpressionTextBox.Text}\n";

                    var result = MathsInterpreterBackend.Parser.interpret(ExpressionTextBox.Text);
                    ResultTextBlock.Text = "= " + result.ToString();

                    DebugTextBlock.Text += $"Parsed result: {result}\n";
                }

                ErrorTextBlock.Text = ""; // Clear any previous error messages
            }
            catch (Exception ex)
            {
                // Display the error message from the backend
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = "";

                // Log error/debug messages in DebugTextBlock
                DebugTextBlock.Text += $"Debug: {ex.Message}\n";

                // Optional: Log stack trace for deeper debugging
                DebugTextBlock.Text += $"Stack Trace: {ex.StackTrace}\n";
            }
        }

    }

}


