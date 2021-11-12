using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class ConnectionRequestDto
    {
        public string Id { get; set; }

        public string Player { get; set; }

        public string PlayerToTargetMessage { get; set; }

        public string CurrentStatus { get; set; }

        public ConnectionRequestDto(string id, string player, string playerToTargetMessage, string currentStatus)
        {
            Id = id;
            Player = player;
            PlayerToTargetMessage = playerToTargetMessage;
            CurrentStatus = currentStatus;
        }
    }
}
