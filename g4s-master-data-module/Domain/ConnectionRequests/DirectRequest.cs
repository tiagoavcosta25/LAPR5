using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.ConnectionRequests
{
    public class DirectRequest : ConnectionRequest
    {

        public DirectRequest()
        {
        }

        public DirectRequest(string player, string target, string playerToTargetMessage, string currentStatus, int strength, ICollection<string> tags) 
            : base(player, target, playerToTargetMessage, currentStatus, strength, tags)
        {
        }
    }
}
