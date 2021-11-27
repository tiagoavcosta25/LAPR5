using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class UpdatingConnectionDto
    {
        public string Id { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public UpdatingConnectionDto(string id, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
