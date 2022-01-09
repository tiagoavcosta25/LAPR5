using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDSample1.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Services
{
    public class PlayerServiceTest
    {

        [Fact]
        public async Task AddAsync_ReturnsAPlayerDto_WithPlayerData()
        {
            // Arrange
            CreatingPlayerDto dto = new CreatingPlayerDto("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            Player obj = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto dto2 = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockRepo = new Mock<IPlayerRepository>();
            mockRepo.Setup(repo => repo.AddAsync(obj));
            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());
            var service = new PlayerService(mockUnity.Object, mockRepo.Object, null);

            // Act
            var result = await service.AddAsync(dto);

            // Assert            
            var returnValue = Assert.IsType<PlayerDto>(result);
            
            Assert.Equal(dto2.Email, returnValue.Email);
        }

        [Fact]
        public async Task UpdateAsync_ReturnsAPlayerDto_WithUpdatedPlayerData()
        {
            // Arrange
            System.Guid id = new System.Guid();
            UpdatePlayerDto dto = new UpdatePlayerDto(id, "mary", "test2@email.com", "321", "911222333", 2000, 2, 19, "distressed", "www.facebook.com/john-doe2", 
            "www.linkedin.com/john-doe2", new List<string>{"tag2"});
            Player obj = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto dto2 = new PlayerDto(id, "mary", "test2@email.com", "911222333", 2000, 2, 19, "distressed", "www.facebook.com/john-doe2", 
            "www.linkedin.com/john-doe2", new List<string>{"tag2"});

            var mockRepo = new Mock<IPlayerRepository>();
            mockRepo.Setup(repo => repo.GetByIdAsync(new PlayerId(dto.Id)))
            .ReturnsAsync(obj).Verifiable();
                var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());
            var service = new PlayerService(mockUnity.Object, mockRepo.Object, null);

            // Act
            var result = await service.UpdateAsync(dto);

            // Assert            
            var returnValue = Assert.IsType<PlayerDto>(result);
            
            Assert.Equal(dto2.DateOfBirth, returnValue.DateOfBirth);
            Assert.Equal(dto2.Email, returnValue.Email);
            Assert.Equal(dto2.EmotionalStatus, returnValue.EmotionalStatus);
            Assert.Equal(dto2.Name, returnValue.Name);
            Assert.Equal(dto2.PhoneNumber, returnValue.PhoneNumber);
            Assert.Equal(dto2.Facebook, returnValue.Facebook);
            Assert.Equal(dto2.LinkedIn, returnValue.LinkedIn);
            Assert.Equal(dto2.Tags, returnValue.Tags);
        }

        
    }
}