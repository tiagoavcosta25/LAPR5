using DDDSample1.Domain.Players;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections
{
    public class UpdateTagsAndStrengthConnectionDTO
    {
        public string Id { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public UpdateTagsAndStrengthConnectionDTO(string id, int connectionStrength, ICollection<string> tags)
        {
            Id = id;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
