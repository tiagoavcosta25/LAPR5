using System.Collections.Generic;
using System;

namespace DDDSample1.Domain.Players
{
    public class GetPlayerSuggestionDto
    {
        public string Email { get;  set; }
        public string Name { get;  set; }
        public string Facebook { get;  private set; }
        public string LinkedIn { get;  private set; }
        public ICollection<string> Tags { get; set; }


        public GetPlayerSuggestionDto(string name, string email, string facebook, string linkedin, ICollection<string> tags)
        {
            this.Name = name;
            this.Email = email;
            this.Facebook = facebook;
            this.LinkedIn = linkedin;
            Tags = tags;
        }
    }
}