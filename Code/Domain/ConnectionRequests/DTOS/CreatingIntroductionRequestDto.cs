namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class CreatingIntroductionRequestDto
    {
        public string Player { get; set; }
        public string MiddleMan { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string PlayerToMiddleManMessage { get; set; }
        public string MiddleManToTargetMessage { get; set; }
        public string CurrentStatus { get; set; }

        public CreatingIntroductionRequestDto(string player, string middleMan, string target, string playerToTargetMessage,
            string playerToMiddleManMessage, string middleManToTargetMessage, string currentStatus)
        {
            Player = player;
            MiddleMan = middleMan;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            PlayerToMiddleManMessage = playerToMiddleManMessage;
            MiddleManToTargetMessage = middleManToTargetMessage;
            CurrentStatus = currentStatus;
        }
    }
}
