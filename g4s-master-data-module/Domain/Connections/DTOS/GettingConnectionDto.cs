using DDDSample1.Domain.Players;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class GettingConnectionDto
    {
        public string Id { get; set; }

        public PlayerDto Player { get; set; }

        public PlayerDto Friend { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public GettingConnectionDto(string id, PlayerDto player, PlayerDto friend, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            Player = player;
            Friend = friend;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
