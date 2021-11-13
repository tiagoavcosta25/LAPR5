using DDDSample1.Domain.Shared;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Domain.Shared
{
    public class Message : IValueObject
    {
        [Required]
        [MaxLength(1000)]
        public string Text { get; private set; }

        public Message(string text)
        {
            Text = text;
        }

        public void ChangeText(string text)
        {
            Text = text;
        }
    }
}
