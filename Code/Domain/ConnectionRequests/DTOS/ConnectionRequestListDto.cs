using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.ConnectionRequests.DTOS
{
    public class ConnectionRequestListDto
    {
        public ICollection<IntroductionRequestDto> IntroductionRequets { get; set; }

        public ICollection<DirectRequestDto> DirectRequests { get; set; }

        public ConnectionRequestListDto(ICollection<IntroductionRequestDto> introductionRequets, ICollection<DirectRequestDto> directRequests)
        {
            IntroductionRequets = introductionRequets;
            DirectRequests = directRequests;
        }
    }
}
