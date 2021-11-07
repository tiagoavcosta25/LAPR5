using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;

namespace DDDSample1.Domain.Profiles
{
    public class ProfileService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IProfileRepository _repo;

        public ProfileService(IUnitOfWork unitOfWork, IProfileRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<ProfileDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<ProfileDto> listDto = list.ConvertAll<ProfileDto>(prof => 
                new ProfileDto(prof.Id.AsGuid(),prof.Name.name, prof.Email.address, prof.PhoneNumber.phoneNumber, prof.DateOfBirth.date.Year, prof.DateOfBirth.date.Month, prof.DateOfBirth.date.Day, prof.EmotionalStatus.Status, prof.Facebook.Url, prof.LinkedIn.Url));

            return listDto;
        }

        public async Task<ProfileDto> GetByIdAsync(ProfileId id)
        {
            var prof = await this._repo.GetByIdAsync(id);
            
            if(prof == null)
                return null;

            return new ProfileDto(prof.Id.AsGuid(),prof.Name.name, prof.Email.address, prof.PhoneNumber.phoneNumber, prof.DateOfBirth.date.Year, prof.DateOfBirth.date.Month, prof.DateOfBirth.date.Day, prof.EmotionalStatus.Status, prof.Facebook.Url, prof.LinkedIn.Url);
        }

        public async Task<ProfileDto> AddAsync(CreatingProfileDto dto)
        {
            var Profile = new Profile(dto.Name, dto.Email, dto.PhoneNumber, dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day, dto.EmotionalStatus, dto.Facebook, dto.LinkedIn);

            await this._repo.AddAsync(Profile);

            await this._unitOfWork.CommitAsync();

            return new ProfileDto(Profile.Id.AsGuid(),Profile.Name.name, Profile.Email.address, Profile.PhoneNumber.phoneNumber, Profile.DateOfBirth.date.Year, Profile.DateOfBirth.date.Month, Profile.DateOfBirth.date.Day, Profile.EmotionalStatus.Status, Profile.Facebook.Url, Profile.LinkedIn.Url);
        }

        public async Task<ProfileDto> UpdateAsync(ProfileDto dto)
        {
            var Profile = await this._repo.GetByIdAsync(new ProfileId(dto.Id)); 

            if (Profile == null)
                return null;   

            Profile.ChangeName(dto.Name);
            Profile.ChangeEmail(dto.Email);
            Profile.ChangePhoneNumber(dto.PhoneNumber);
            Profile.ChangeDateOfBirth(dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day);
            Profile.ChangeEmotionalStatus(dto.EmotionalStatus);
            Profile.ChangeFacebook(dto.Facebook);
            Profile.ChangeLinkedIn(dto.LinkedIn);
            
            await this._unitOfWork.CommitAsync();

            return new ProfileDto(Profile.Id.AsGuid(),Profile.Name.name, Profile.Email.address, Profile.PhoneNumber.phoneNumber, Profile.DateOfBirth.date.Year, Profile.DateOfBirth.date.Month, Profile.DateOfBirth.date.Day, Profile.EmotionalStatus.Status, Profile.Facebook.Url, Profile.LinkedIn.Url);
        }

        public async Task<ProfileDto> InactivateAsync(ProfileId id)
        {
            var Profile = await this._repo.GetByIdAsync(id); 

            if (Profile == null)
                return null;   

            Profile.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new ProfileDto(Profile.Id.AsGuid(),Profile.Name.name, Profile.Email.address, Profile.PhoneNumber.phoneNumber, Profile.DateOfBirth.date.Year, Profile.DateOfBirth.date.Month, Profile.DateOfBirth.date.Day, Profile.EmotionalStatus.Status, Profile.Facebook.Url, Profile.LinkedIn.Url);
        }

        public async Task<ProfileDto> DeleteAsync(ProfileId id)
        {
            var Profile = await this._repo.GetByIdAsync(id); 

            if (Profile == null)
                return null;   

            if (Profile.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active Profile.");
            
            this._repo.Remove(Profile);
            await this._unitOfWork.CommitAsync();

            return new ProfileDto(Profile.Id.AsGuid(),Profile.Name.name, Profile.Email.address, Profile.PhoneNumber.phoneNumber, Profile.DateOfBirth.date.Year, Profile.DateOfBirth.date.Month, Profile.DateOfBirth.date.Day, Profile.EmotionalStatus.Status, Profile.Facebook.Url, Profile.LinkedIn.Url);
        }
    }
}