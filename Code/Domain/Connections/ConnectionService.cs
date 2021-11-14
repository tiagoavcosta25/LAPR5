using DDDNetCore.Domain.Connections.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IConnectionRepository _repo;

        private readonly IPlayerRepository _repoPl;

        public ConnectionService(IUnitOfWork unitOfWork, IConnectionRepository repo, IPlayerRepository repoPl)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repoPl = repoPl;
        }

        public async Task<List<ConnectionDto>> GetAllAsync()
        {
            var list = await _repo.GetAllAsync();

            List<ConnectionDto> listDto = list.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            return listDto;
        }

        public async Task<ConnectionDto> GetByIdAsync(ConnectionId id)
        {
            var con = await _repo.GetByIdAsync(id);

            if (con == null)
                return null;

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> AddAsync(CreatingConnectionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Friend));
            var con = new Connection(dto.Player.ToString(), dto.Friend.ToString(), dto.ConnectionStrength, dto.Tags);

            await _repo.AddAsync(con);

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), 0, new List<string>());
        }

        public async Task<ConnectionDto> UpdateAsync(ConnectionDto dto)
        {
            await checkPlayerIdAsync(new PlayerId(dto.Player));
            await checkPlayerIdAsync(new PlayerId(dto.Friend));
            var con = await _repo.GetByIdAsync(new ConnectionId(dto.Id));

            if (con == null)
                return null;

            con.ChangePlayer(dto.Player);
            con.ChangeFriend(dto.Friend);
            con.ChangeConnectionStrength(dto.ConnectionStrength);
            con.ChangeTags(dto.Tags);

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> InactivateAsync(ConnectionId id)
        {
            var con = await _repo.GetByIdAsync(id);

            if (con == null)
                return null;

            con.MarkAsInactive();

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> DeleteAsync(ConnectionId id)
        {
            var con = await _repo.GetByIdAsync(id);

            if (con == null)
                return null;

            _repo.Remove(con);
            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        private async Task checkPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");
        }


        // CRUD OVER //

        public async Task<List<GettingConnectionDto>> GetAllConnectionsAsync(string playerEmail)
        {
            var pl = await _repoPl.GetByEmailAsync(playerEmail);

            if(pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Email.");

            var list = await _repo.GetAllUserConnectionsAsync(pl.Id);

            List<ConnectionDto> listDto = list.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            List<GettingConnectionDto> finalListDto = new List<GettingConnectionDto>();

            foreach (var con in listDto)
            {
                var player = await _repoPl.GetByIdAsync(new PlayerId(con.Friend));
                finalListDto.Add(new GettingConnectionDto(player.Name.name, player.Email.address, con.ConnectionStrength, con.Tags));
            }

            return finalListDto;
        }

        public async Task<ConnectionDto> GetByEmailsAsync(string playerEmail, string friendEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);
            var friend = await _repoPl.GetByEmailAsync(friendEmail);

            if (player == null || friend == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Email");

            var con = await _repo.GetByBothPlayerIdAsync(player.Id, friend.Id);

            if (con == null)
                return null;

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }


        public async Task<ConnectionDto> UpdateTagsAndStrengthAsync(UpdatingConnectionDto dto)
        {

            var player = await _repoPl.GetByEmailAsync(dto.PlayerEmail);
            var friend = await _repoPl.GetByEmailAsync(dto.FriendEmail);

            if (player == null || friend == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Email");

            var con = await _repo.GetByBothPlayerIdAsync(player.Id, friend.Id);

            if (con == null)
                return null;

            con.ChangeConnectionStrength(dto.ConnectionStrength);
            con.ChangeTags(dto.Tags);

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<List<PlayerDto>> GetReachablePlayers(string playerEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);

            var friendsList = await _repo.GetFriendsList(player.Id);

            List<Player> reachableUsersList = new List<Player>();

            foreach(PlayerId id in friendsList){
                var lst = await _repo.GetFriendsList(player.Id);
                reachableUsersList.AddRange(await _repoPl.GetByIdsAsync(lst));
            }

            return reachableUsersList.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, 
                plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));
        }

        public async Task<List<PlayerDto>> GetMutualFriends(string playerEmail, PlayerDto targetDto)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);
            var target = await _repoPl.GetByEmailAsync(targetDto.Email);

            var lst = await _repo.GetMutualFriends(player.Id, target.Id);

            var mutualfriendsList = await _repoPl.GetByIdsAsync(lst);

            return mutualfriendsList.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, 
                plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), plyr.Facebook.Url, plyr.LinkedIn.Url));

        }
        
        public async Task<List<ConnectionDto>> GetNetwork(PlayerId id, int scope){
            
            List<Connection> lst = new List<Connection>();
            lst = await this.GetNetwork(id, scope, lst);

            List<ConnectionDto> lstDto = lst.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            return lstDto;
        }

        private async Task<List<Connection>> GetNetwork(PlayerId id, int scope, List<Connection> lst){
            
            if(scope < 1){
                return lst;
            }

            List<Connection> lstFriends = await this._repo.GetAllUserConnectionsAsync(id);

            lst.AddRange(lstFriends);
            
            foreach(Connection c in lstFriends){
                await this.GetNetwork(c.Friend, scope - 1);
            }

            return lst;
        }

    }
}
