using System.Collections.Generic;
using System;

namespace DDDSample1.Domain.Players
{
    public class GetPlayerDto
    {
        public string Email { get;  set; }
        public string Name { get;  set; }
        public string PhoneNumber { get;  set; }
        public DateTime DateOfBirth { get;  set; }
        public string EmotionalStatus { get;  private set; }
        public string Facebook { get;  private set; }
        public string LinkedIn { get;  private set; }
        public ICollection<string> Tags { get; set; }


        public GetPlayerDto(string name, string email, string phoneNumber, int year, int month, int day, string emotionalStatus, 
        string facebook, string linkedin, ICollection<string> tags)
        {
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.DateOfBirth = new DateTime(year, month, day);
            this.EmotionalStatus = emotionalStatus;
            this.Facebook = facebook;
            this.LinkedIn = linkedin;
            Tags = tags;
        }
    }
}