using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class ConnectionDto
    {
        public string Id { get; set; }

        public string Player { get; set; }

        public string Friend { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public ConnectionDto(string id, string player, string friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Player = player;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
