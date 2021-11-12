using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class GettingConnectionDto
    {
        public string FriendName { get; set; }

        public string FriendEmail { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public GettingConnectionDto(string friendName, string friendEmail, int connectionStrength, ICollection<string> tags)
        {
            FriendName = friendName;
            FriendEmail = friendEmail;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
