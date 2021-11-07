using DDDSample1.Domain.Profiles;
using System;

namespace DDDSample1.Domain.Profiles
{
    public class CreatingProfileDto
    {
        public string Email { get;  set; }
        public string Name { get;  set; }
        public double PhoneNumber { get;  set; }
        public DateTime DateOfBirth { get;  set; }
        public string EmotionalStatus { get;  private set; }
        public string Facebook { get;  private set; }
        public string LinkedIn { get;  private set; }


        public CreatingProfileDto(string name, string email, double phoneNumber, int year, int month, int day, string emotionalStatus, string facebook, string linkedin)
        {
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.DateOfBirth = new DateTime(year, month, day);
            this.EmotionalStatus = emotionalStatus;
            this.Facebook = facebook;
            this.LinkedIn = linkedin;
        }
    }
}