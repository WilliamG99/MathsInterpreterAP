using System;
using System.Windows;
using System.Windows.Controls;
using MathsInterpreterBackend;
using Microsoft.FSharp.Collections;

namespace MathsInterpreterGUI
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            string expression = ExpressionTextBox.Text;

            try
            {
                var tokens = MathsInterpreterBackend.Lexer.lexer(expression);
                var resultTuple = MathsInterpreterBackend.Parser.parseNeval(tokens);
                int result = ((Tuple<FSharpList<MathsInterpreterBackend.Lexer.terminal>, int>)resultTuple).Item2;

                ResultTextBlock.Text = $"Result: {result}";
                ErrorTextBlock.Text = "";
            }
            catch (Exception ex)
            {
                ErrorTextBlock.Text = $"Error: {ex.Message}";
                ResultTextBlock.Text = "";
            }
        }

        private void ExpressionTextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            PlaceholderText.Visibility = string.IsNullOrEmpty(ExpressionTextBox.Text) ? Visibility.Visible : Visibility.Hidden;
        }
    }
}
