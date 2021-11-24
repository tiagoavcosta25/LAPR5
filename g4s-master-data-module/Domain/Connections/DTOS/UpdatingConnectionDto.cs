using System.Collections.Generic;

namespace DDDNetCore.Domain.Connections.DTOS
{
    public class UpdatingConnectionDto
    {
        public string PlayerEmail { get; set; }

        public string FriendEmail { get; set; }

        public int ConnectionStrength { get; set; }

        public ICollection<string> Tags { get; set; }

        public UpdatingConnectionDto(string playerEmail, string friendEmail, int connectionStrength, ICollection<string> tags)
        {
            PlayerEmail = playerEmail;
            FriendEmail = friendEmail;
            ConnectionStrength = connectionStrength;
            Tags = tags;
        }
    }
}
