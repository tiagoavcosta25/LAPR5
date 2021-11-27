using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Connections.DTOS;

namespace DDDNetCore.Domain.Connections
{
    public interface IConnectionService
    {

        Task<List<ConnectionDto>> GetAllAsync();

        Task<ConnectionDto> GetByIdAsync(ConnectionId id);

        Task<ConnectionDto> AddAsync(CreatingConnectionDto dto);

        Task<ConnectionDto> UpdateAsync(ConnectionDto dto);

        Task<ConnectionDto> InactivateAsync(ConnectionId id);

        Task<ConnectionDto> DeleteAsync(ConnectionId id);

        Task<List<GettingConnectionDto>> GetAllConnectionsAsync(string playerId);

        Task<ConnectionDto> GetByEmailsAsync(string playerEmail, string friendEmail);

        Task<ConnectionDto> UpdateTagsAndStrengthAsync(UpdatingConnectionDto dto);

        Task<List<PlayerDto>> GetReachablePlayers(string playerEmail);

        Task<List<PlayerDto>> GetMutualFriends(string playerEmail, GetMutualFriendsDto targetDto);

        Task<List<ConnectionDto>> GetNetwork(string playerEmail, GetNetworkDto dto);
    }
}
