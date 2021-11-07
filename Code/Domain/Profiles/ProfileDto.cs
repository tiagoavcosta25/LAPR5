using System;
using DDDSample1.Domain.Categories;

namespace DDDSample1.Domain.Profiles
{
    public class ProfileDto
    {
        public Guid Id { get; set; }
        public string Email { get;  set; }
        public string Name { get;  set; }
        public double PhoneNumber { get;  set; }
        public DateTime DateOfBirth { get;  set; }

        public ProfileDto(Guid Id, string name, string email, double phoneNumber, int year, int month, int day)
        {
            this.Id = Id;
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.DateOfBirth = new DateTime(year, month, day);
        }
    }
}