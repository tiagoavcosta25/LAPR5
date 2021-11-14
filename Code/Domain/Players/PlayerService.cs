using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Players;
using System;

namespace DDDSample1.Domain.Players
{
    public class PlayerService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPlayerRepository _repo;

        public PlayerService(IUnitOfWork unitOfWork, IPlayerRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<PlayerDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<PlayerDto> listDto = list.ConvertAll<PlayerDto>(plyr => 
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));

            return listDto;
        }

        public async Task<PlayerDto> GetByIdAsync(PlayerId id)
        {
            var plyr = await this._repo.GetByIdAsync(id);
            
            if(plyr == null)
                return null;

            return new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url);
        }

        public async Task<PlayerDto> AddAsync(CreatingPlayerDto dto)
        {
            var Player = new Player(dto.Name, dto.Email, dto.PhoneNumber, dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day, dto.EmotionalStatus, dto.Facebook, dto.LinkedIn);

            await this._repo.AddAsync(Player);

            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url);
        }

        public async Task<PlayerDto> UpdateAsync(PlayerDto dto)
        {
            var Player = await this._repo.GetByIdAsync(new PlayerId(dto.Id)); 

            if (Player == null)
                return null;   

            Player.ChangeName(dto.Name);
            Player.ChangeEmail(dto.Email);
            Player.ChangePhoneNumber(dto.PhoneNumber);
            Player.ChangeDateOfBirth(dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day);
            Player.ChangeEmotionalStatus(dto.EmotionalStatus);
            Player.ChangeFacebook(dto.Facebook);
            Player.ChangeLinkedIn(dto.LinkedIn);
            
            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url);
        }

        public async Task<PlayerDto> InactivateAsync(PlayerId id)
        {
            var Player = await this._repo.GetByIdAsync(id); 

            if (Player == null)
                return null;   

            Player.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url);
        }

        public async Task<PlayerDto> DeleteAsync(PlayerId id)
        {
            var Player = await this._repo.GetByIdAsync(id); 

            if (Player == null)
                return null;   

            if (Player.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active Player.");
            
            this._repo.Remove(Player);
            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url);
        }


        // CRUD OVER
        public async Task<ChangeEmotionalStatusDto> ChangeEmotionalStatusAsync(ChangeEmotionalStatusDto dto)
        {
            var Player = await this._repo.GetByEmailAsync(dto.PlayerEmail);

            if (Player == null)
                return null;
            var exists = Enum.TryParse(dto.EmotionalStatus, out OOC _);
            if (!exists)
            {
                throw new BusinessRuleValidationException("Not a valid emotional status.");
            }

            Player.ChangeEmotionalStatus(dto.EmotionalStatus);

            await this._unitOfWork.CommitAsync();


            return new ChangeEmotionalStatusDto(Player.Email.address, Player.EmotionalStatus.Status.ToString());
        }

        public async Task<CreatingPlayerDto> GetByEmailAsync(string email)
        {
            var plyr = await this._repo.GetByEmailAsync(email);

            if (plyr == null)
                return null;

            return new CreatingPlayerDto(plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url);
        }

        public async Task<List<CreatingPlayerDto>> GetByNameAsync(string name)
        {
            var list = await _repo.GetByNameAsync(name);

            List<CreatingPlayerDto> listDto = list.ConvertAll<CreatingPlayerDto>(plyr =>
                new CreatingPlayerDto(plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));

            return listDto;
        }

        public async Task<List<CreatingPlayerDto>> GetByPhoneAsync(string phoneNumber)
        {
            var list = await _repo.GetByPhoneAsync(phoneNumber);

            List<CreatingPlayerDto> listDto = list.ConvertAll<CreatingPlayerDto>(plyr =>
                new CreatingPlayerDto(plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));

            return listDto;
        }

        public async Task<List<CreatingPlayerDto>> GetByTagAsync(string tag)
        {
            var list = await _repo.GetByTagAsync(tag);

            List<CreatingPlayerDto> listDto = list.ConvertAll<CreatingPlayerDto>(plyr =>
                new CreatingPlayerDto(plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));

            return listDto;
        }


    }
}