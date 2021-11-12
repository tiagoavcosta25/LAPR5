namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class CreatingDirectRequestDto
    {
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string CurrentStatus { get; set; }

        public CreatingDirectRequestDto(string player, string target, string playerToTargetMessage,
            string currentStatus)
        {
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            CurrentStatus = currentStatus;
        }
    }
}
