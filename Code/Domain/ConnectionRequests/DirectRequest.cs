namespace DDDNetCore.Domain.ConnectionRequests
{
    public class DirectRequest : ConnectionRequest
    {
        public DirectRequest(string player, string target, string playerToTargetMessage, string currentStatus) : base(player, target, playerToTargetMessage, currentStatus)
        {
        }
    }
}
