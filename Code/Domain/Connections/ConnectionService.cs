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
            var con = new Connection(dto.Player.ToString(), dto.Friend.ToString());

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

        private async Task checkPlayerEmailAsync(string playerEmail) 
        {
            var pl = await _repoPl.GetByEmailAsync(playerEmail);
            if (pl == null)
                throw new BusinessRuleValidationException("Invalid Player or Friend Email.");
        }


        // CRUD OVER //

        public async Task<List<GettingConnectionDto>> GetAllConnectionsAsync(string playerEmail)
        {
            await checkPlayerEmailAsync(playerEmail);

            var pl = await _repoPl.GetByEmailAsync(playerEmail);

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


        public async Task<ConnectionDto> UpdateTagsAndStrengthAsync(UpdatingConnectionDto dto)
        {
            await checkPlayerEmailAsync(dto.PlayerEmail);
            await checkPlayerEmailAsync(dto.FriendEmail);

            var player = await _repoPl.GetByEmailAsync(dto.PlayerEmail);
            var friend = await _repoPl.GetByEmailAsync(dto.FriendEmail);

            var con = await _repo.GetByBothPlayerIdAsync(player.Id, friend.Id);

            if (con == null)
                return null;

            con.ChangeConnectionStrength(dto.ConnectionStrength);
            con.ChangeTags(dto.Tags);

            await _unitOfWork.CommitAsync();

            return new ConnectionDto(con.Id.AsString(), con.Player.AsString(), con.Friend.AsString(), con.ConnectionStrength.Strength, con.Tags.Select(t => t.tagName).ToList());
        }

    }
}
