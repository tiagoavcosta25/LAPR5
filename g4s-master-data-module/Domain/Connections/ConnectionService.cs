using DDDNetCore.Domain.Connections.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.Connections
{
    public class ConnectionService : IConnectionService
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
            await CheckPlayerIdAsync(new PlayerId(dto.Player));
            await CheckPlayerIdAsync(new PlayerId(dto.Friend));
            var con = new Connection(dto.Player.ToString(), dto.Friend.ToString(), dto.ConnectionStrength, dto.Tags);

            await _repo.AddAsync(con);

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

        public async Task<ConnectionDto> UpdateAsync(ConnectionDto dto)
        {
            await CheckPlayerIdAsync(new PlayerId(dto.Player));
            await CheckPlayerIdAsync(new PlayerId(dto.Friend));
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

        private async Task CheckPlayerIdAsync(PlayerId playerId)
        {
            var pl = await _repoPl.GetByIdAsync(playerId);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");
        }


        // CRUD OVER //

        public async Task<List<GettingConnectionDto>> GetAllConnectionsAsync(string email)
        {
            var pl = await _repoPl.GetByEmailAsync(email);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Id.");

            await CheckPlayerIdAsync(pl.Id);

            var list = await _repo.GetAllUserConnectionsAsync(pl.Id);

            List<ConnectionDto> listDto = list.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            List<GettingConnectionDto> finalListDto = new List<GettingConnectionDto>();

            foreach (var con in listDto)
            {
                var fr = await _repoPl.GetByIdAsync(new PlayerId(con.Friend));

                var pDto = new PlayerDto(pl.Id.AsGuid(), pl.Name.name, pl.Email.address, pl.PhoneNumber.phoneNumber, pl.DateOfBirth.date.Year, 
                    pl.DateOfBirth.date.Month, pl.DateOfBirth.date.Day, pl.EmotionalStatus.Status.ToString(), pl.Facebook.Url, pl.LinkedIn.Url, pl.Tags.Select(t => t.tagName).ToList());

                var frDto = new PlayerDto(fr.Id.AsGuid(), fr.Name.name, fr.Email.address, fr.PhoneNumber.phoneNumber, fr.DateOfBirth.date.Year,
                    fr.DateOfBirth.date.Month, fr.DateOfBirth.date.Day, fr.EmotionalStatus.Status.ToString(), fr.Facebook.Url, fr.LinkedIn.Url, fr.Tags.Select(t => t.tagName).ToList());

                finalListDto.Add(new GettingConnectionDto(con.Id.ToString(), pDto, frDto, con.ConnectionStrength, con.Tags));
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

            var con = await _repo.GetByIdAsync(new ConnectionId(dto.Id));

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

            List<PlayerId> friendsList = await _repo.GetFriendsList(player.Id);

            List<Player> reachableUsersList = new List<Player>();
            List<PlayerId> lstTotal = new List<PlayerId>();

            foreach(PlayerId id in friendsList){
                var lst = await _repo.GetFriendsList(id);
                foreach(PlayerId temp in lst){
                    if(!lstTotal.Contains(temp) && temp != player.Id){
                        lstTotal.Add(temp);
                    }
                }
            }
            reachableUsersList.AddRange(await _repoPl.GetByIdsAsync(lstTotal));

            return reachableUsersList.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, 
                plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), 
                plyr.Facebook.Url, plyr.LinkedIn.Url, plyr.Tags.Select(t => t.tagName).ToList()));
        }

        public async Task<List<PlayerDto>> GetMutualFriends(string playerEmail, string friendEmail)
        {
            var player = await _repoPl.GetByEmailAsync(playerEmail);
            var target = await _repoPl.GetByEmailAsync(friendEmail);

            var lst = await _repo.GetMutualFriends(player.Id, target.Id);

            var mutualfriendsList = await _repoPl.GetByIdsAsync(lst);

            return mutualfriendsList.ConvertAll<PlayerDto>(plyr =>
                new PlayerDto(plyr.Id.AsGuid(),plyr.Name.name, plyr.Email.address, plyr.PhoneNumber.phoneNumber, 
                plyr.DateOfBirth.date.Year, plyr.DateOfBirth.date.Month, plyr.DateOfBirth.date.Day, plyr.EmotionalStatus.Status.ToString(), 
                plyr.Facebook.Url, plyr.LinkedIn.Url, plyr.Tags.Select(t => t.tagName).ToList()));

        }
        
        public async Task<List<ConnectionDto>> GetNetwork(string playerEmail, int scope){

            var player = await _repoPl.GetByEmailAsync(playerEmail);
            
            List<Connection> lst = new();
            List<PlayerId> lstIds = new();
            lst = await this.GetNetwork(player.Id, scope, lst, lstIds);

            List<ConnectionDto> lstDto = lst.ConvertAll<ConnectionDto>(con =>
                new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList()));

            return lstDto;
        }

        private async Task<List<Connection>> GetNetwork(PlayerId id, int scope, List<Connection> lst, List<PlayerId> lstIds){
            
            if(scope < 1 || lstIds.Contains(id)){
                return lst;
            }

            lstIds.Add(id);

            List<Connection> lstFriends = await this._repo.GetAllUserConnectionsAsync(id);

            foreach(Connection con in lstFriends) {
                if (!lst.Exists(x => x.Player.AsString().Equals(con.Friend.AsString()))) {
                    lst.Add(con);
                }
            }
            
            foreach(Connection c in lstFriends){
                await this.GetNetwork(c.Friend, scope - 1, lst, lstIds);
            }

            return lst;
        }

    }
}
