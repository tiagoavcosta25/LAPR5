namespace DDDSample1.Domain.Players
{
    public class ChangeEmotionalStatusDto
    {
        public string PlayerEmail { get; set; }
        public string EmotionalStatus { get; set; }

        public ChangeEmotionalStatusDto(string playerEmail, string emotionalStatus)
        {
            PlayerEmail = playerEmail;
            EmotionalStatus = emotionalStatus;
        }
    }
}
