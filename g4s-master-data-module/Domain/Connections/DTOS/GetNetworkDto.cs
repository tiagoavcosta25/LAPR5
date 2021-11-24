using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class GetNetworkDto
    {
        public int Scope { get; set; }

        public GetNetworkDto(int scope)
        {
            Scope = scope;
        }
    }
}
