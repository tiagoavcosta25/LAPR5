using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class ListMidPendingRequestDto
    {
        public string Player { get; set; }
        public string MiddleMan { get; set; }
        public string Target { get; set; }
        public string PlayerToMiddleManMessage { get; set; }

        public ListMidPendingRequestDto(string player, string middleMan, string target, string playerToMiddleManMessage)
        {
            Player = player;
            MiddleMan = middleMan;
            Target = target;
            PlayerToMiddleManMessage = playerToMiddleManMessage;
        }
    }
}
