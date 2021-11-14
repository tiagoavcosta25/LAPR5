using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Players
{
    public class GetMutualFriendsDto
    {
        public string Email { get;  set; }

        public GetMutualFriendsDto(string email)
        {
            this.Email = email;
        }
    }
}