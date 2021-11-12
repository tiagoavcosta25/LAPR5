using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.Shared
{
    public class Message : IValueObject
    {
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
