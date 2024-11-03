using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using MathsInterpreterBackend;

namespace MathsInterpreterGUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            // Example of calling F# code
            var result = Interpreter.add(5, 10); // Calls the F# add function
            MessageBox.Show(result.ToString());
        }

        private void ProcessButton_Click(object sender, RoutedEventArgs e)
        {
            // Get the input from the TextBox
            string expression = ExpressionTextBox.Text;

            // Call F# function to evaluate the expression
            try
            {
                // F# function "EvaluateExpression"
                var result = MathsInterpreterBackend.Parser.ParseExpression(expression);
                ResultTextBlock.Text = $"Result: {result}";
            }
            catch (Exception ex) 
            {
                ResultTextBlock.Text = $"Error: {ex.Message}";
            }
        }
    }
}