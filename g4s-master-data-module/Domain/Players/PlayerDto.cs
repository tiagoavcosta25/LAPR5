using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Players
{
    public class PlayerDto
    {
        public Guid Id { get; set; }
        public string Email { get;  set; }
        public string Name { get;  set; }
        public string Avatar { get;  set; }
        public string PhoneNumber { get;  set; }
        public DateTime DateOfBirth { get;  set; }
        public string EmotionalStatus { get;  private set; }
        public string Facebook { get;  private set; }
        public string LinkedIn { get;  private set; }
        public ICollection<string> Tags { get; set; }

        public PlayerDto(Guid Id, string name, string email, string avatar, string phoneNumber, int year, int month, int day, string emotionalStatus, string facebook, 
        string linkedin, ICollection<string> tags)
        {
            this.Id = Id;
            this.Name = name;
            this.Email = email;
            this.Avatar = avatar;
            this.PhoneNumber = phoneNumber;
            this.DateOfBirth = new DateTime(year, month, day);
            this.EmotionalStatus = emotionalStatus;
            this.Facebook = facebook;
            this.LinkedIn = linkedin;
            Tags = tags;
        }
    }
}