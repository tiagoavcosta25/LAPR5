using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.Connections;
using DDDNetCore.Domain.Connections.DTOS;
using DDDSample1.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;
using System.Linq;

namespace DDDNetCore.Tests.Services
{
    public class ConnectionServiceTest
    {
        [Fact]
        public async Task GetReachablePlayers_ReturnsPlayersList()
        {
            // Arrange
            Player plyr = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            Player plyr2 = new Player("john2", "test2@email.com", "1232", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag2"});
            Player plyr3 = new Player("john3", "test3@email.com", "1233", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag3"});
            PlayerDto dto2 = new PlayerDto(new System.Guid(), "john3", "test3@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag3"});
            string playerEmail = "test2@email.com";
            List<PlayerId> lstId = new List<PlayerId>{plyr2.Id};
            List<PlayerId> lstId2 = new List<PlayerId>{plyr3.Id, plyr.Id};
            List<PlayerId> lstId3 = new List<PlayerId>{plyr3.Id};
            List<Player> lst = new List<Player>{plyr3};


            var mockRepo = new Mock<IConnectionRepository>();
            mockRepo.Setup(repo => repo.GetFriendsList(plyr.Id))
                .ReturnsAsync(lstId).Verifiable();
            mockRepo.Setup(repo => repo.GetFriendsList(plyr2.Id))
                .ReturnsAsync(lstId2).Verifiable();

            var mockRepoPl = new Mock<IPlayerRepository>();
            mockRepoPl.Setup(repo => repo.GetByEmailAsync(playerEmail))
                .ReturnsAsync(plyr).Verifiable();
            mockRepoPl.Setup(repo => repo.GetByIdsAsync(lstId3))
                .ReturnsAsync(lst).Verifiable();

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());

            var service = new ConnectionService(mockUnity.Object, mockRepo.Object, mockRepoPl.Object);

            // Act
            var result = await service.GetReachablePlayers(playerEmail);

            // Assert            
            var returnValue = Assert.IsType<List<PlayerDto>>(result);
            
            Assert.Equal(dto2.Email, returnValue.First().Email);
        }

        [Fact]
        public async Task GetMutualFriends_ReturnsPlayersList()
        {
            // Arrange
            Player plyr = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            Player plyr2 = new Player("john2", "test2@email.com", "1232", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag2"});
            Player plyr3 = new Player("john3", "test3@email.com", "1233", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag3"});
            GetMutualFriendsDto dto = new GetMutualFriendsDto(plyr3.Email.address.ToString());
            PlayerDto dto2 = new PlayerDto(plyr2.Id.AsGuid(), "john", "test2@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag3"});
            List<PlayerId> lstId = new List<PlayerId>{plyr2.Id};
            List<Player> lst = new List<Player>{plyr2};


            var mockRepo = new Mock<IConnectionRepository>();
            mockRepo.Setup(repo => repo.GetMutualFriends(plyr.Id, plyr3.Id))
                .ReturnsAsync(lstId).Verifiable();

            var mockRepoPl = new Mock<IPlayerRepository>();
            mockRepoPl.Setup(repo => repo.GetByEmailAsync(plyr.Email.address.ToString()))
                .ReturnsAsync(plyr).Verifiable();
            mockRepoPl.Setup(repo => repo.GetByEmailAsync(plyr3.Email.address.ToString()))
                .ReturnsAsync(plyr3).Verifiable();
            mockRepoPl.Setup(repo => repo.GetByIdsAsync(lstId))
                .ReturnsAsync(lst).Verifiable();

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());

            var service = new ConnectionService(mockUnity.Object, mockRepo.Object, mockRepoPl.Object);

            // Act
            var result = await service.GetMutualFriends(plyr.Email.address.ToString(), plyr3.Email.address.ToString());

            // Assert            
            var returnValue = Assert.IsType<List<PlayerDto>>(result);
            
            Assert.Equal(dto2.Email, returnValue.First().Email);
        }

        [Fact]
        public async Task GetNetwork_ReturnsConnectionList()
        {
            // Arrange
            Player plyr = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            Player plyr2 = new Player("john2", "test2@email.com", "1232", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag2"});
            Player plyr3 = new Player("john3", "test3@email.com", "1233", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag3"});
            GetNetworkDto dto = new GetNetworkDto(2);
            Connection con = new Connection(plyr.Id.AsString(), plyr2.Id.AsString(), 1, new List<string>{"tag1"});
            Connection con2 = new Connection(plyr2.Id.AsString(), plyr3.Id.AsString(), 1, new List<string>{"tag2"});

            ConnectionDto connDto = new ConnectionDto(con.Id.AsString(), plyr.Id.AsString(), plyr2.Id.AsString(), 1, new List<string>{"tag1"});
            ConnectionDto connDto2 = new ConnectionDto(con2.Id.AsString(), plyr2.Id.AsString(), plyr3.Id.AsString(), 1, new List<string>{"tag2"});
            List<Connection> lst = new List<Connection>{con};
            List<Connection> lst2 = new List<Connection>{con2};
            


            var mockRepo = new Mock<IConnectionRepository>();
            mockRepo.Setup(repo => repo.GetAllUserConnectionsAsync(plyr.Id))
                .ReturnsAsync(lst).Verifiable();
            mockRepo.Setup(repo => repo.GetAllUserConnectionsAsync(plyr2.Id))
                .ReturnsAsync(lst2).Verifiable();

            var mockRepoPl = new Mock<IPlayerRepository>();
            mockRepoPl.Setup(repo => repo.GetByEmailAsync(plyr.Email.address.ToString()))
                .ReturnsAsync(plyr).Verifiable();

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());

            var service = new ConnectionService(mockUnity.Object, mockRepo.Object, mockRepoPl.Object);

            // Act
            var result = await service.GetNetwork(plyr.Email.address.ToString(), 2);

            // Assert            
            var returnValue = Assert.IsType<List<ConnectionDto>>(result);
            
            Assert.Equal(connDto.Friend, returnValue.First().Friend);
            Assert.Equal(connDto.Player, returnValue.First().Player);
            Assert.Equal(connDto.ConnectionStrength, returnValue.First().ConnectionStrength);
            Assert.Equal(connDto.Tags, returnValue.First().Tags);
        }

        [Fact]
        public async Task AddAsync_ReturnsAConnectionDto_WithConnectionData()
        {
            // Arrange
            Player p = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            Player p2 = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            

            CreatingConnectionDto dto = new CreatingConnectionDto(p.Id.AsString(), p2.Id.AsString(),1, new List<string>{"tag1"});
            Connection con = new Connection(dto.Player.ToString(), dto.Friend.ToString(), dto.ConnectionStrength, dto.Tags);
            ConnectionDto dto2 = new ConnectionDto("12312322-4444-5555-6666-777888999000", p.Id.AsString(),p2.Id.AsString(), 
            1, new List<string>{"tag1"});

            var mockRepo = new Mock<IConnectionRepository>();
            mockRepo.Setup(repo => repo.AddAsync(con));

            var mockRepoPl = new Mock<IPlayerRepository>();
            mockRepoPl.Setup(repo => repo.GetByIdAsync(p.Id))
                .ReturnsAsync(p).Verifiable();
            mockRepoPl.Setup(repo => repo.GetByIdAsync(p2.Id))
                .ReturnsAsync(p2).Verifiable();

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());
            var service = new ConnectionService(mockUnity.Object, mockRepo.Object, mockRepoPl.Object);

            // Act
            var result = await service.AddAsync(dto);

            // Assert            
            var returnValue = Assert.IsType<ConnectionDto>(result);
            
            Assert.Equal(dto.Friend, returnValue.Friend);
            Assert.Equal(dto.Player, returnValue.Player);
            Assert.Equal(dto.ConnectionStrength, returnValue.ConnectionStrength);
            Assert.Equal(dto.Tags, returnValue.Tags);
        }
        
    }
}