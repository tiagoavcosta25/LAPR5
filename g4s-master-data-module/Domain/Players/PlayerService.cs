using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using System.Linq;
using System;
using DDDNetCore.Domain.Shared;
using Microsoft.AspNetCore.SignalR;

namespace DDDSample1.Domain.Players
{
    public class PlayerService : IPlayerService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPlayerRepository _repo;
        private readonly IHubContext<AppHub> _hub;

        public PlayerService(IUnitOfWork unitOfWork, IPlayerRepository repo, IHubContext<AppHub> hub)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            _hub = hub;
        }

        public async Task<List<PlayerDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<PlayerDto> listDto = list.ConvertAll<PlayerDto>(plyr => 
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, 
                plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
                plyr.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<PlayerDto> GetByIdAsync(PlayerId id)
        {
            var plyr = await this._repo.GetByIdAsync(id);
            
            if(plyr == null)
                return null;

            return new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, 
            plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
            plyr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<PlayerDto> AddAsync(CreatingPlayerDto dto)
        {
            var Player = new Player(dto.Name, dto.Email, dto.Password, dto.Avatar, dto.PhoneNumber, dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day, dto.EmotionalStatus, 
            dto.Facebook, dto.LinkedIn, dto.Tags);

            await this._repo.AddAsync(Player);

            await this._unitOfWork.CommitAsync();

            var number = await _repo.GetNumberOfPlayers();

            await _hub.Clients.All.SendAsync("playerPost", number);

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.Avatar.url, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, 
            Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url,
            Player.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<int> getPlayerNumber() 
        {
            var number = await _repo.GetNumberOfPlayers();
            return number;
        }

        public async Task<PlayerDto> UpdateAsync(UpdatePlayerDto dto)
        {
            var Player = await this._repo.GetByIdAsync(new PlayerId(dto.Id)); 

            if (Player == null)
                return null;   

            Player.ChangeName(dto.Name);
            Player.ChangeEmail(dto.Email);
            Player.ChangeAvatar(dto.Avatar);
            Player.ChangePhoneNumber(dto.PhoneNumber);
            Player.ChangeDateOfBirth(dto.DateOfBirth.Year, dto.DateOfBirth.Month, dto.DateOfBirth.Day);
            Player.ChangeEmotionalStatus(dto.EmotionalStatus);
            Player.ChangeFacebook(dto.Facebook);
            Player.ChangeLinkedIn(dto.LinkedIn);
            Player.ChangePassword(dto.Password);
            Player.ChangeTags(dto.Tags);
            
            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.Avatar.url, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, 
            Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url,
            Player.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<PlayerDto> InactivateAsync(PlayerId id)
        {
            var Player = await this._repo.GetByIdAsync(id); 

            if (Player == null)
                return null;   

            Player.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.Avatar.url, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, 
            Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url,
            Player.Tags.Select(t => t.tagName).ToList());
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

            return new PlayerDto(Player.Id.AsGuid(),Player.Name.name, Player.Email.address, Player.Avatar.url, Player.PhoneNumber.phoneNumber, Player.DateOfBirth.date.Year, 
            Player.DateOfBirth.date.Month, Player.DateOfBirth.date.Day, Player.EmotionalStatus.Status.ToString(), Player.Facebook.Url, Player.LinkedIn.Url,
            Player.Tags.Select(t => t.tagName).ToList());
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

        public async Task<PlayerDto> GetByEmailAsync(string email)
        {
            var plyr = await this._repo.GetByEmailAsync(email);

            if (plyr == null)
                return null;

            return new PlayerDto(plyr.Id.AsGuid(), plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, 
            plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
            plyr.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<List<PlayerDto>> GetByNameAsync(string name)
        {
            var list = await _repo.GetByNameAsync(name);

            List<PlayerDto> listDto = list.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(), plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, 
                plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
                plyr.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<List<PlayerDto>> GetByPhoneAsync(string phoneNumber)
        {
            var list = await _repo.GetByPhoneAsync(phoneNumber);

            List<PlayerDto> listDto = list.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(), plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, 
                plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
                plyr.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<List<PlayerDto>> GetByTagAsync(string tag)
        {
            var list = await _repo.GetByTagAsync(tag);

            List<PlayerDto> listDto = list.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(), plyr.Name.name, plyr.Email.address, plyr.Avatar.url, plyr.PhoneNumber.phoneNumber, plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, 
                plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url,
                plyr.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }


        public ICollection<string> GetFilters()
        {
            ICollection<string> filters = new List<string>
            {
                UserSearchFilterEnum.email.ToString(),
                UserSearchFilterEnum.name.ToString(),
                UserSearchFilterEnum.phone.ToString(),
                UserSearchFilterEnum.tag.ToString()
            };
            return filters;
        }

        public async Task<List<GetPlayerSuggestionDto>> GetSuggestions(string playerEmail)
        {
            var list = await _repo.GetSuggestions(playerEmail);

            List<GetPlayerSuggestionDto> listDto = list.ConvertAll<GetPlayerSuggestionDto>(plyr =>
                new GetPlayerSuggestionDto(plyr.Name.name, plyr.Email.address, plyr.Facebook.Url, plyr.LinkedIn.Url,
                plyr.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<int> Login(string playerEmail, string playerPassword)
        {
            var player = await _repo.GetByEmailAsync(playerEmail);
            if (player == null) {
                return 3;
            } 
            
            if (player.Password.password.Equals(playerPassword) && player.Active) {
                return 1;
            }

            return 2;
        }

    }
}