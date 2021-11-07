using DDDSample1.Domain.Categories;

namespace DDDSample1.Domain.Profiles
{
    public class CreatingProfileDto
    {
        public string Email { get;  set; }
        public string Name { get;  set; }
        public string PhoneNumber { get;  set; }
        public DateTime DateOfBirth { get;  set; }


        public CreatingProfileDto(string name, string email, double phoneNumber, int year, int month, int day)
        {
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.DateOfBirth = new DateTime(year, month, day);
            this.CategoryId = catId;
        }
    }
}