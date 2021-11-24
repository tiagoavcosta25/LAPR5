using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class ApproveRequestDto
    {
        public string Status { get; set; }
        public string MiddleManToTargetMessage { get; set; }

        public ApproveRequestDto(string status, string middleManToTargetMessage)
        {
            Status = status;
            MiddleManToTargetMessage = middleManToTargetMessage;
        }
    }
}
