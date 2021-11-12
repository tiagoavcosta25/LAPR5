using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class DirectRequestDto
    {
        public string Id { get; set; }
        public string Player { get; set; }
        public string Target { get; set; }
        public string PlayerToTargetMessage { get; set; }
        public string CurrentStatus { get; set; }

        public DirectRequestDto(string id, string player, string target, string playerToTargetMessage,
            string currentStatus)
        {
            Id = id;
            Player = player;
            Target = target;
            PlayerToTargetMessage = playerToTargetMessage;
            CurrentStatus = currentStatus;
        }
    }
}
